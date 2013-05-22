#define CP_BYPASS_INSTR_NAME(FUNC) FUNC##_BYPASS_CALL

#define CP_BYPASS_INSTR_NAME_STR(FUNC) #FUNC"_BYPASS_CALL"

#define CP_BYPASS_INSTR_DEFINE(FUNC) \
	extern void* CP_BYPASS_INSTR_NAME(FUNC);

#define CP_CALL_BYPASS_REG(FUNC) \
	CP_BYPASS_INSTR_DEFINE(FUNC) \
	CodePatcherRegBypassInstr(&CP_BYPASS_INSTR_NAME(FUNC));

#define __PUSH_GP "push %%rax\t\n"\
"push %%rbx\t\n"\
"push %%rcx\t\n"\
"push %%rdx\t\n"\
"push %%rbp\t\n"\
"push %%rsp\t\n"\
"push %%rsi\t\n"\
"push %%rdi\t\n"\
"push %%r8\t\n"\
"push %%r9\t\n"\
"push %%r10\t\n"\
"push %%r11\t\n"\
"push %%r12\t\n"\
"push %%r13\t\n"\
"push %%r14\t\n"\
"push %%r15\t\n"

#define __POP_GP "pop %%r15\t\n"\
"pop %%r14\t\n"\
"pop %%r13\t\n"\
"pop %%r12\t\n"\
"pop %%r11\t\n"\
"pop %%r10\t\n"\
"pop %%r9\t\n"\
"pop %%r8\t\n"\
"pop %%rdi\t\n"\
"pop %%rsi\t\n"\
"pop %%rsp\t\n"\
"pop %%rbp\t\n"\
"pop %%rdx\t\n"\
"pop %%rcx\t\n"\
"pop %%rbx\t\n"\
"pop %%rax\t\n"

#define __CP_CALL(FUNC, CALL_SPECIFIC_ASM, CALL_SPECIFIC_INPUT) \
	asm __volatile__ (".align 4\t\n"\
			".globl "CP_BYPASS_INSTR_NAME_STR(FUNC)"\t\n"\
			CP_BYPASS_INSTR_NAME_STR(FUNC)":\t\n"\
			".byte 0xe9 #jmp to 2:\t\n"\
			".4byte 2f - 1f + 3\t\n"\
			".byte 0x0f, 0x1f, 0x00\t\n"\
			"1:\t\n"\
			__PUSH_GP\
			"pushf \t\n"\
			CALL_SPECIFIC_ASM \
			"popf \t\n"\
			__POP_GP \
			"2:\t\n" ::CALL_SPECIFIC_INPUT:"memory");

//===================== 1 arg =====================
#define __CPC1_ASM(FUNC, arg1) "movq %0, %%rdi\t\n"\
"call "#FUNC"\t\n"

#define __CPC1_INPUT(arg1) "r"((void*))arg1

#define CP_BYPASSABLE_CALL1(FUNC, arg1) \
	__CP_CALL(FUNC, __CPC1_ASM(FUNC), __CPC1_INPUT(arg1))

//===================== 2 arg =====================
#define __CPC2_ASM(FUNC) "movq %0, %%rdi\t\n"\
"movq %1, %%rsi\t\n"\
"call "#FUNC"\t\n"

#define __CPC2_INPUT(arg1, arg2) "r"((void*)arg1), "r"((void*)arg2)

#define CP_BYPASSABLE_CALL2(FUNC, arg1, arg2) \
	__CP_CALL(FUNC, __CPC2_ASM(FUNC), __CPC2_INPUT(arg1, arg2))

//===================== 3 arg =====================
void CodePatcherInit(void);
void CodePatcherRegBypassInstr(void **instrAddr);
void PathsOn(void);
void PathsOff(void);
