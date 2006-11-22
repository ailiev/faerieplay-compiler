// template file for the helper code, to run SFDL-targeted C++ on normal g++
// etc.
// placeholders filled in by GenHelper_C.hs

#include <iostream>
#include <stdlib.h>

using namespace std;

%s				// sfdlmain return type **rettype_d**
sfdlmain (%s			// sfdlmain param declarations **sfdlmain_arg_decls_d**
    );

int parse_args (int argc, const char* argv[],
		%s		// sfdlmain output params **parse_outargs_decl_d**
    )
{
    int rc;
    
    if (argc < %s		// number of args needed **num_main_args**
	)
    {
	cerr << argv[0] << ": Too few args!" << endl;
	return 1;
    }

    // parse argv into the sfdlmain arg addresses. **arg_parses_d**
    %s;

    return rc;
}



int main (int argc, const char* argv[])
{
    // sfdlmain param variables **main_param_var_decls_d**
    %s;
    //
    
    int res;
    int err;

    err = parse_args (argc, argv,
		      %s	// addresses of sfdlmain params:
				// **main_param_var_addrs_d**
	);

    if (err != 0) {
	cerr << "parse_args error " << err << endl;
	exit (err);
    }

    res = sfdlmain (%s		// sfdlmain params **main_params_d**
	);

    cout << res << endl;
}
