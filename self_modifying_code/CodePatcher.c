#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#include <errno.h>
#include "CodePatcher.h"

#define BYPASS_PATCH_BYTE_SIZE 8
typedef struct {
	void *addr;
	char oldVal[BYPASS_PATCH_BYTE_SIZE];
} PatchInstruction;

typedef struct {
	PatchInstruction instrs[BYPASS_PATCH_BYTE_SIZE];
	unsigned int instrCount;
} BypassInstrsSwitcher;
static BypassInstrsSwitcher bypassInstrsSwitcher;

/* Patch application order:
 * 1. Put LoopPatch to patching instruction: all cpus that haven't reached 
 *    patching instruction yet will loop on it.
 *    All cpus that reached patching instruction will observe 5 bytes nop and continue
 *    with next unchanged 3 bytes nop.
 * 2. Atomically write with exchg instruction last 4 bytes of Patch or OldVal. Loopers still don't observe
 *    changes.
 * 3. Atomically write with exchg instruction first 4 bytes of Patch or OldVal. Loopers observe all changes
 *    and continue.
 * Should work on x86_32 and x86_64
 */
static char loopPatch[] = {0xeb, -2, 0x00, 0x00}; //loop here instruction
static char patch[] = {0x0f, 0x1f, 0x44, 0x00, 0x00, 0x0f, 0x1f, 0x00}; //5 bytes nop then 3 bytes nop

void CodePatcherInit(void)
{
	memset(&bypassInstrsSwitcher, 0, sizeof(bypassInstrsSwitcher));	
}

#define PAGE_SHIFT      ((unsigned int)12)
#define PAGE_SIZE       (((unsigned int)1) << PAGE_SHIFT)
#define PAGE_MASK       (~(PAGE_SIZE-1))
#define PAGE_ALIGN(addr)((void*)((size_t)addr & PAGE_MASK))

void CodePatcherRegBypassInstr(void **instrAddr)
{
	int ret = 0;
	printf("%s: %p\n", __FUNCTION__, instrAddr);
	PatchInstruction *instr = &bypassInstrsSwitcher.instrs[bypassInstrsSwitcher.instrCount++];
	instr->addr = (void*)instrAddr;
	memcpy(instr->oldVal, instr->addr, BYPASS_PATCH_BYTE_SIZE); 
	ret = mprotect(PAGE_ALIGN(instr->addr), BYPASS_PATCH_BYTE_SIZE, 
			PROT_READ | PROT_WRITE | PROT_EXEC);
	if (ret)
	{
		fprintf(stderr, "Couldn't mprotect addr: %p", instr->addr);
		perror("");
	}
}

void PathsOn(void)
{
	int i = 0;
	for(; i != bypassInstrsSwitcher.instrCount; ++i)
	{
		PatchInstruction *instr = &bypassInstrsSwitcher.instrs[i];
		//Apply patch in patch application order
		asm __volatile__("movl (%1), %%eax\t\n"
				"xchgl %%eax, (%0)\t\n"
				"movl 4(%2), %%eax\t\n"
				"xchgl %%eax, 4(%0)\t\n"
				"movl (%2), %%eax\t\n"
				"xchgl %%eax, (%0)\t\n"
			::"r"(instr->addr), "r"(loopPatch), "r"(patch):"eax"/*, "memory"*/);
	}
}

void PathsOff(void)
{
	int i = 0;
	for(; i != bypassInstrsSwitcher.instrCount; ++i)
	{
		PatchInstruction *instr = &bypassInstrsSwitcher.instrs[i];
		//Apply patch in patch application order
		asm __volatile__("movl (%1), %%eax\t\n"
				"xchgl %%eax, (%0)\t\n"
				"movl 4(%2), %%eax\t\n"
				"xchgl %%eax, 4(%0)\t\n"
				"movl (%2), %%eax\t\n"
				"xchgl %%eax, (%0)\t\n"
			::"r"(instr->addr), "r"(loopPatch), "r"(instr->oldVal):"eax"/*, "memory"*/);
	}
}
