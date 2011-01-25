#include "sfdl-crutches.h"

program(gcd);

const int WordSize = 32;

typedef int Word;

function Word gcd (Word a, Word b)
{
    var int temp, i;

    for (i = 1; i <= WordSize*2; i = i + 1) {
	if (b != 0) {
	    temp = b;
	    b	 = a % b;
	    a	 = temp;
	}
    }

    return a;
}


function int sfdlmain (int a, int b)
{
    return gcd (a, b);
}
