#include <stdlib.h>
#include <stdio.h>
#include <gmp.h>
#include <gc.h>
#include <string.h>
#include <inttypes.h>
#include "getline.h"

extern char** environ;

void putStr(const char *str) {
  fputs(str, stdout);
}

void putErr(const char *str) {
  fputs(str, stderr);
}

void mpz_init_set_ull(mpz_t n, unsigned long long ull)
{
  mpz_init_set_ui(n, (unsigned int)(ull >> 32)); /* n = (unsigned int)(ull >> 32) */
  mpz_mul_2exp(n, n, 32);                   /* n <<= 32 */
  mpz_add_ui(n, n, (unsigned int)ull);      /* n += (unsigned int)ull */
}

void mpz_init_set_sll(mpz_t n, long long sll)
{
  mpz_init_set_si(n, (int)(sll >> 32));     /* n = (int)sll >> 32 */
  mpz_mul_2exp(n, n, 32 );             /* n <<= 32 */
  mpz_add_ui(n, n, (unsigned int)sll); /* n += (unsigned int)sll */
}

void mpz_set_sll(mpz_t n, long long sll)
{
  mpz_set_si(n, (int)(sll >> 32));     /* n = (int)sll >> 32 */
  mpz_mul_2exp(n, n, 32 );             /* n <<= 32 */
  mpz_add_ui(n, n, (unsigned int)sll); /* n += (unsigned int)sll */
}

unsigned long long mpz_get_ull(mpz_t n)
{
  unsigned int lo, hi;
  mpz_t tmp;

  mpz_init( tmp );
  mpz_mod_2exp( tmp, n, 64 );   /* tmp = (lower 64 bits of n) */

  lo = mpz_get_ui( tmp );       /* lo = tmp & 0xffffffff */ 
  mpz_div_2exp( tmp, tmp, 32 ); /* tmp >>= 32 */
  hi = mpz_get_ui( tmp );       /* hi = tmp & 0xffffffff */

  mpz_clear( tmp );

  return (((unsigned long long)hi) << 32) + lo;
}

char *__idris_strCons(char c, char *s) {
  size_t len = strlen(s);
  char *result = GC_malloc_atomic(len+2);
  result[0] = c;
  memcpy(result+1, s, len);
  result[len+1] = 0;
  return result;
}


char *__idris_readStr(FILE* h) {
    char *buffer = NULL;
	size_t n = 0;
	ssize_t len = 0;

	len = getline(&buffer, &n, h);
	strtok(buffer, "\n");

	if (len <= 0) {
		return "";
	} else {
		return buffer; 
	}
}

char* __idris_readChars(int len, FILE* h) {
    char* buffer = (char*) GC_malloc(len);
    char* read = fgets(buffer, len, h);
    if (read == 0) buffer[0] = 0;
    return buffer;
}


int __idris_writeStr(void* h, char* str) {
    FILE* f = (FILE*)h;
    if (fputs(str, f) >= 0) {
        return 0;
    } else {
        return -1;
    }
}

void* __idris_registerPtr(void* p, int size) {
    void* mp = GC_malloc(size);
    return memcpy(mp, p, size);
}


int __idris_sizeofPtr(void) {
    return sizeof((void*)0);
}

// stdin and friends are often macros, so let C handle that problem. 
FILE* __idris_stdin() {
    return stdin;
}

FILE* __idris_stdout() {
    return stdout;
}
FILE* __idris_stderr() {
    return stderr;
}

void* fileOpen(char* name, char* mode) {
  FILE* f = fopen(name, mode);
  return (void*)f;
}

void fileClose(void* h) {
  FILE* f = (FILE*)h;
  fclose(f);
}

int fileEOF(void* h) {
  FILE* f = (FILE*)h;
  return feof(f);
}

int fileError(void* h) {
  FILE* f = (FILE*)h;
  return ferror(f);
}

void fputStr(void* h, char* str) {
  FILE* f = (FILE*)h;
  fputs(str, f);
}

int isNull(void* ptr) {
  return ptr==NULL;
}

char* getEnvPair(int i) {
    return *(environ + i);
}

void idris_memset(void* ptr, size_t offset, uint8_t c, size_t size) {
  memset(((uint8_t*)ptr) + offset, c, size);
}

uint8_t idris_peek(void* ptr, size_t offset) {
  return *(((uint8_t*)ptr) + offset);
}

void idris_poke(void* ptr, size_t offset, uint8_t data) {
  *(((uint8_t*)ptr) + offset) = data;
}

void idris_memmove(void* dest, void* src, size_t dest_offset, size_t src_offset, size_t size) {
  memmove(dest + dest_offset, src + src_offset, size);
}

void *__idris_gmpMalloc(size_t size) {
  return GC_malloc(size);
}

void *__idris_gmpRealloc(void *ptr, size_t oldSize, size_t size) {
  return GC_realloc(ptr, size);
}

void __idris_gmpFree(void *ptr, size_t oldSize) {
  GC_free(ptr);
}

char *__idris_strRev(const char *s) {
  int x = strlen(s);
  int y = 0;
  char *t = GC_malloc(x+1);

  t[x] = '\0';
  while(x>0) {
    t[y++] = s[--x];
  }
  return t;
}

int __idris_argc;
char **__idris_argv;

int idris_numArgs() {
    return __idris_argc;
}

const char* idris_getArg(int i) {
    return __idris_argv[i];
}
