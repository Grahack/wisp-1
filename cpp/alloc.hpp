#pragma once

#include <limits>

#if true // for now
#include <cstdlib>
#define GC_NEW(Type) new (malloc(sizeof(Type))) Type
#define GC_ALLOC malloc
#else
#include <gc.h>
#define GC_NEW(Type) new (GC_MALLOC(sizeof(Type))) Type
#define GC_ALLOC GC_MALLOC
#endif

