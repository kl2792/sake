/* Usage examples:
 * DECLARE_BITVECTOR(vector, 1024); // 1024-bit vector initialized to all 0s
 * SETBIT(vector, 1023);           // set the last bit in the vector
 * CLRBIT(vector, 1023);           // unset the last bit 
 * TRYBIT(vector, 1023) == 0;      // get the boolean value of the last bit
 *
 * DECLARE_BITMATRIX(matrix, 7, 42);// array of 7 42-bit bitvectors
 * SETBIT(matrix[3], 41);           // set the 41st bit of the 4th vector
 * Inspired by PantryFS (www.cs.columbia.edu/~jae/4118/HW7-pantry.html)
 * Author: Kai-Zhan Lee
 */

#ifndef __BITVECTOR_H__
#define __BITVECTOR_H__

#define DECLARE_BITVECTOR(v, sz) unsigned int v[(sz) / 32 + 1] = { 0 }
#define DECLARE_BITMATRIX(m, n, sz) unsigned int m[(sz) / 32 + 1][n] = { 0 }
#define SETBIT(v, i) ((v)[(i) / 32] |=  (1 << ((i) % 32)))
#define CLRBIT(v, i) ((v)[(i) / 32] &= ~(1 << ((i) % 32)))
#define TRYBIT(v, i) ((v)[(i) / 32] &   (1 << ((i) % 32)))

#endif
