#!/usr/bin/env python3

# Copyright (C) 2019, Francois Berenger
#
# Yamanishi laboratory,
# Department of Bioscience and Bioinformatics,
# Faculty of Computer Science and Systems Engineering,
# Kyushu Institute of Technology,
# 680-4 Kawazu, Iizuka, Fukuoka, 820-8502, Japan.

import sys, rdkit, time
from rdkit import Chem
from rdkit.Chem import AllChem
from optparse import OptionParser

def RobustSmilesMolSupplier(filename):
    with open(filename) as f:
        for line in f:
            words = line.split()
            smile = words[0]
            name = " ".join(words[1:]) # everything after the SMILES
            yield (Chem.MolFromSmiles(smile), name)

            # CLI setup -------------------------------------------------------------------
parser = OptionParser(usage = """Usage: %prog -i in.smi \
[-b nb_bits (def=2048)] [-r radius (def=3)] [--sparse] > out.ecfp6""")
parser.add_option("-i", action = "store", type = "string", dest = "in_fn")
parser.add_option("-b", action = "store", type = "int", dest = "nb_bits",
                  default = 2048) # rdkit's default
parser.add_option("-r", action = "store", type = "int", dest = "radius",
                  default = 3) # ECFP6
# output in sparse format
parser.add_option("--sparse", action = "store_true", dest = "sparse_fmt")

# main ------------------------------------------------------------------------
if __name__ == '__main__':
    before = time.time()
    (opts, args) = parser.parse_args()
    if len(sys.argv) == 1:
        parser.print_help()
        exit(0)
    input_fn = opts.in_fn
    nb_bits = opts.nb_bits
    assert(nb_bits == 1024 or nb_bits == 2048 or nb_bits == 16384)
    fp_radius = opts.radius
    assert(fp_radius == 2 or fp_radius == 3)
    sparse_output = opts.sparse_fmt
    ok_count = 0
    ko_count = 0
    for mol, name in RobustSmilesMolSupplier(input_fn):
        if mol:
            ok_count += 1
            if (ok_count % 1000) == 0:
                # user feedback
                print("done: %d" % ok_count, end='\r',
                      file=sys.stderr, flush=True)
            # ECFP6 2048 bits
            fp = AllChem.GetMorganFingerprintAsBitVect(mol, fp_radius,
                                                       nBits = nb_bits)
            bits = fp.ToBitString()
            if sparse_output:
                assert(False)
            else:
                # this format can be read by molenc's pubchem_decoder
                print("%s,0.0,%s" % (name, bits))
        else:
            ko_count += 1
    after = time.time()
    dt = after - before
    total = ok_count + ko_count
    print("%d molecules (%d errors) at %.2f mol/s" %
          (total, ko_count, total / dt), file=sys.stderr)
