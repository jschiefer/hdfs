#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "float_circuit.h"

void print_float_bits32(FILE *f, unsigned int val) {
    int i;
    for (i=0; i<32; i++) {
        int bit = 32-i-1;
        fprintf(f, "%c", val & (1<<(bit & 31)) ? '1' : '0');
        if (i == 0 || i == 8) fprintf(f, " ");
    }    
}

void print_bits(FILE *f, unsigned long *val, int nbits) {
    int i;
    for (i=0; i<nbits; i++) {
        int bit = nbits-i-1;
        fprintf(f, "%c", val[bit >> 5] & (1<<(bit & 31)) ? '1' : '0');
    }
}

int count_zeros(unsigned long long a, int b) {
    int i;
    int zeros=0;
    for (i=b-1; i>=0; i--) {
        if (a & (1LL<<i)) break;
        zeros ++;
    }
    return zeros;
}

int is_nan(unsigned int a) {
    unsigned int ea = (a >> 23) & 0xff;
    unsigned int ma = a & 0x7FFFFF;
    return (ea == 0xff) && (ma != 0);
}

int nans_same(unsigned int a, unsigned int b) {
    return ((a>>22)&1) == ((b>>22)&1);
}

int flt2int(float a) {
    int *p = (int *) &a;
    return *p;
}

float int2flt(int a) {
    float *p = (float *) &a;
    return *p;
}

unsigned int round754(unsigned int mantissa, unsigned int rnd, unsigned int sticky) {
    int r = (rnd & 2) && ( (rnd & 1) || sticky || (mantissa & 1) );
    return mantissa + r;
}

unsigned int mask21 = 0x1fffff;
unsigned int mask22 = 0x3fffff;
unsigned int mask23 = 0x7fffff;

// Perform rounding, adjust exp on overflow 
void round(unsigned int *mant, unsigned int rnd, unsigned int sticky, unsigned int *exp) {
    // If the rounding operation causes overflow, set mantissa to zero and increment exp.
    // What then happens if exp overflows?  *If* it's already in range so it will go to inf (not nan because of zero-ed mantissa).
    int r = (rnd & 2) && ( (rnd & 1) || sticky || (mantissa & 1) );
    if (r) {
        if (*mant == mask23) {  // Rather than check for all ones we could just do the add and test the carry bit
            *exp = *exp + 1;
            *mant = 0;
        } else {
            *mant = *mant + 1;
        }
    }
}

unsigned int normalise(unsigned long long mant) {
    int lz = count_zeros(mant, 48);
    int shift;
    
    // Align the 1st one at the 47th bit.  
    // This could be a shift by 1 to the right, or a shift left.
    // (The shift could be simplified to a left shift by padding the vector with a 0 before counting zeros and taking a slice shited 1 to the left)
    if (lz == 0) {
        // shift right by one
        mant >>= 1;
        shift = -1;
    } else {
        // shift left by 0 or more.  At some point this will cause exponent overflow
        shift = lz - 1;
        mant <<= shift;
    }
    
    // Mantissa is now correctly aligned.  
    // Bottom 24 bits are for rounding.
    sticky = (mant & mask22) != 0;
    rnd = (mant >> 22) & 3;
    mant = (mant >> 24) & mask23;
    
    return ;
}

unsigned int float_mul(unsigned int a, unsigned int b, int verb) {
    unsigned int sa = a >> 31;
    unsigned int sb = b >> 31;
    unsigned int ea = (a >> 23) & 0xff;
    unsigned int eb = (b >> 23) & 0xff;
    unsigned int da = ea == 0;
    unsigned int db = eb == 0;
    unsigned long long ma = (a & 0x7FFFFF) | (da ? 0 : 1<<23);
    unsigned long long mb = (b & 0x7FFFFF) | (db ? 0 : 1<<23);
    unsigned long long mul;
    unsigned int sign = sa ^ sb;
    unsigned int mantissa = 0;
    unsigned int leading_zeros;
    
    int expa = ea - 127;        // What about denormal?
    int expb = eb - 127;
    int exp = expa + expb;
    
    mul = ma * mb;
    
    leading_zeros = count_zeros(mul, 48);
    
    if (verb) {
        //printf("exps = %i %i\n", ea, eb);
        printf("exp: %i + %i = %i\n", expa, expb, exp);
        printf("den: a = %i, b = %i\n", da, db);
        printf("man: ");
        print_bits(stdout, (unsigned long*) &mul, 48);
        printf("\n");
    }

    if (is_nan(a) || is_nan(b)) {
        exp = 255;
        mantissa = 1<<22;              // Set to signalling nan.  Should be qnan?
    } else if (exp < -126) {           // Result is denormal.  This is the tricky bit
        unsigned int stickybits;
        unsigned int rnd;
        unsigned int sticky;
        
        // Any 2 denormal numbers will result in a zero result, with appropriate sign
        int shift = -127-exp;
        //printf("denormal exp = %i, shift is %i\n", exp, shift);
        // shift to normalise relative to exponent
        mul >>= shift;
        // get rounding bits
        stickybits = mul & mask21;
        rnd = (mul >> 21) & 0x3;
        sticky = stickybits != 0;
        // drop fraction bits
        mantissa = ((mul >> 23) & mask23);
        // round
        if (verb) printf("rnd: mant&1 = %i, rnd = %i, sticky = %i\n", mantissa & 1, rnd, sticky);
        mantissa = round754(mantissa, rnd, sticky);
        exp = 0;            
    } else if (exp > 127) {
        // Overflow.  Make inf (sign is copied through)
        exp = 255;
        mantissa = 0;
    } else {
        // 48 bit result.
        // [][     23              ][    23               ]
        // iifffffffffffffffffffffffrrsssssssssssssssssssss
        unsigned int stickybits, rnd, sticky;
        
        exp += (da | db);   // Adjust exponent if one argument is denormal (if both are denormal we deal with them above)

        // One of the numbers is denormal
        if (da | db) {
            mantissa = ((mul >> 21) & mask23);
            stickybits = mul & mask21;
            rnd = (mul >> 19) & 0x3;
        } else if (mul & (1LL<<47)) {
            exp = exp + 1;
            mantissa = ((mul >> 24) & mask23);
            stickybits = mul & mask22;
            rnd = (mul >> 22) & 0x3;
        } else {
            mantissa = ((mul >> 23) & mask23);
            stickybits = mul & mask21;
            rnd = (mul >> 21) & 0x3;
        }
        sticky = stickybits != 0;

        // round
        if (verb) {
            printf("rnd: mant&1 = %i, rnd = %i, sticky = %i\n", mantissa & 1, rnd, sticky);
            printf("lzs: %i\n", leading_zeros);
        }
        mantissa = round754(mantissa, rnd, sticky);
        
        exp = (exp + 127) & 0xff;
    }
    return (sign << 31) | (exp << 23) | mantissa;
}

unsigned int float_add(unsigned int a, unsigned int b, int verb) {
}

void set_from_float(float f, float *fo, int *io) {
    *fo = f;
    *io = flt2int(f);
}

void set_from_int(int i, float *fo, int *io) {
    *fo = int2flt(i);
    *io = i;
}

int test_op(int op, unsigned int ia, unsigned int ib, float fa, float fb, int verb) {
    FILE *f = stdout;
    float fr = op ? fa * fb : fa + fb;
    unsigned int ir = op ? float_mul(ia, ib, verb) : float_add(ia, ib, verb);

    if (verb) {
        fprintf(f, "  "); print_float_bits32(f, ia);           fprintf(f, " : %f\n", int2flt(ia));
        fprintf(f, "+ "); print_float_bits32(f, ib);           fprintf(f, " : %f\n", int2flt(ib));
        fprintf(f, "~ "); print_float_bits32(f, ir);           fprintf(f, " : %f\n", int2flt(ir));
        fprintf(f, "= "); print_float_bits32(f, flt2int(fr));  fprintf(f, " : %f\n", fr);
    }
    
    unsigned int ifr = flt2int(fr);
    if (ir != ifr) {
        // We tolerate the values begin different if they are nans and encode the same nan type
        if (is_nan(ir) && is_nan(ifr) && nans_same(ir,ifr)) return 1;
        else return 0;
    }
    else return 1;
}

unsigned long rand32() {
    unsigned long a = rand()&0xffff;
    unsigned long b = rand()&0xffff;
    return a | (b<<16);
}

void test_ops(simulator_t sim, unsigned int a, unsigned int b) {
    signal_t sa = find_simulator(sim, "add");
    signal_t sm = find_simulator(sim, "mul");
    float fa = int2flt(a);
    float fb = int2flt(b);
    
    if (sa.signal) 
        if (!test_op(0, a, b, fa, fb, 0)) {
            printf("******************************************************************\n");
            printf("Add failed.\n");
            test_op(0, a, b, fa, fb, 1);
        }
    
    if (sm.signal) 
        if (!test_op(1, a, b, fa, fb, 0)) {
            printf("******************************************************************\n");
            printf("Mul failed.\n");
            test_op(1, a, b, fa, fb, 1);
        }
    
}


int main(int argc, char *argv[]) {
    
    //int ai, bi, ci;
    //float a, b, c;
    //set_from_int(0x80730c93, &a, &ai);
    //set_from_int(0x4ee6abc6, &b, &bi);
    //set_from_float(1.0, &a, &ai);
    //set_from_float(1.0, &b, &bi);
    
    int i;
    unsigned int ia, ib;
    simulator_t sim;

    switch (argc) {
    // If there are no arguments, run a regression test for 1000 values
    case 1:
        for (i=0; i<1000; i++) test_ops(sim, rand32(), rand32());
        break;
    // If there is one argument, run a regression test for the given number of values
    case 2:
        for (i=0; i<atoi(argv[1]); i++) test_ops(sim, rand32(), rand32());
        break;
    // If there are 2 arguments run a single test
    case 3:
        test_ops(sim, flt2int(atof(argv[1])), flt2int(atof(argv[2])));
        break;
    default:
        printf("Invalid number of arguments\n");
        return 0;
    }
    

    return 0;    
}
