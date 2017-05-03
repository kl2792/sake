#ifndef __BITVECTOR_H__
#define __BITVECTOR_H__

#define DEFINE_BITVECTOR(v, size) unsigned int v[(size) / 32 + 1]  
#define SETBIT(v, i) (v[(i) / 32] |=  (1 << ((k) % 32)))
#define CLRBIT(v, i) (v[(i) / 32] &= ~(1 << ((k) % 32)))
#define TRYBIT(v, i) (v[(i) / 32] &   (1 << ((k) % 32)))

#endif
