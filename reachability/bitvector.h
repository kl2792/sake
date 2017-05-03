/* Usage examples:
 * DEFINE_BITVECTOR(vector, 1024); // initialized to all 0s
 * SETBIT(vector, 1023);           // set the last bit in the vector
 * CLRBIT(vector, 1023);           // unset the last bit 
 * TRYBIT(vector, 1023) == 0;      // get the boolean value of the last bit
 *
 * Inspired by PantryFS (http://www.cs.columbia.edu/~jae/4118/HW7-pantry.html)
 * Author: Kai-Zhan Lee
 */

#ifndef __BITVECTOR_H__
#define __BITVECTOR_H__

#define DEFINE_BITVECTOR(v, size) unsigned int v[(size) / 32 + 1] = { 0 }
#define SETBIT(v, i) (v[(i) / 32] |=  (1 << ((k) % 32)))
#define CLRBIT(v, i) (v[(i) / 32] &= ~(1 << ((k) % 32)))
#define TRYBIT(v, i) (v[(i) / 32] &   (1 << ((k) % 32)))

#endif
