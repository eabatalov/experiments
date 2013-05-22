#include <stdio.h>

typedef struct {
	unsigned int stuff;
} TLSData;
TLSData tls_data[1];

int main() {
	unsigned int stuff;
	asm volatile("movl %%gs:%1, %0\n\t"
		: "=r"(stuff) : "m"(tls_data[0].stuff));
	printf("TLS: %x\n", stuff);
	return 0;
}
