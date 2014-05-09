#ifndef HSFFI_H
#define HSFFI_H

#include <stdint.h>

typedef uint32_t HsChar;
typedef signed int HsInt;
typedef unsigned int HsWord;

typedef void (*HsFunPtr)(void);

typedef void *HsPtr;
typedef void *HsForeignPtr;
typedef void *HsStablePtr;

#define HS_BOOL_FALSE 0
#define HS_BOOL_TRUE 1

extern void hs_init(int *argc, char **argv[]);
extern void hs_exit(void);
extern void hs_add_root(void (*init_root)(void));

#endif // HSFFI_H
