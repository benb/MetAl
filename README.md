MetAl is a command-line utilty for calculating metric distances between
alternative alignments of the same sequences.

## Download

Binaries are available for Windows, Linux and Mac on [Github](https://github.com/benb/EvoHaskell/downloads).

## Usage

Run `metal -h` for usage. There are four metrics, detailed in this paper:

   Measuring the distance between multiple sequence alignments 
   Blackburne, B.P. and Whelan, S.
   Bioinformatics
   [doi:10.1093/bioinformatics/btr701](http://dx.doi.org/10.1093/bioinformatics/btr701)


The default metric, d-pos, should be fine for most uses. Additional information
may be generated by using the other metrics too, see the paper for details.

Metal accepts Fasta and "relaxed Phylip" format. In case of doubt, use Fasta.

## Examples

Some example alignments of the dataset BB12036 from BaliBASE are available in
the `example' directory.

How different are alignments from ClustalW and Prank, using metric d-pos?

   $ metal example/BB12036.prank-fas example/BB12036.clustalw-fas
   2636 / 17604 = 0.14973869575096568

So The ClustalW and Prank alignments are ~15% different.

How do different sequences contribute to the distance?

   $ metal -a example/BB12036.prank-fas example/BB12036.clustalw-fas
   1qf9_A 382 / 2328 = 0.1640893470790378
   1uky_ 378 / 2352 = 0.16071428571428573
   1zak_A 782 / 2640 = 0.2962121212121212
   1zip_ 402 / 2604 = 0.1543778801843318
   2ak2_ 472 / 2640 = 0.1787878787878788
   2ak3_A 480 / 2712 = 0.17699115044247787
   3adk_ 390 / 2328 = 0.16752577319587628
   3286 / 17604 = 0.18666212224494433

Clearly, 1zak_A has the greatest discrepency between the two aligners.

What are the differences between the alignments from ProbCons and T-COFFEE?

   $ metal example/BB12036.probcons-fas example/BB12036.tcoffee-fas
   392 / 17604 = 2.226766643944558e-2

And using the --tree metric with a reference tree (d-evol in the paper):

   $ metal -t example/BB12036.tre example/BB12036.probcons-fas example/BB12036.tcoffee-fas 
   424 / 17604 = 2.4085435128379913e-2
 
Both ProbCons and T-COFFEE are consistency-based aligners and so we should not
be surprised that the alignments they produce are sometimes relatively similar.

The BaliBASE reference alignment is given as example/BB12036.balibase-ref-fas.
We can rank the difference alignments in order of distance to the reference
like so:

   $ for i in example/*fas; do ANS=$(metal $i example/BB12036.balibase-ref-fas); echo $i $ANS ;done | sort -g -k 6 
   example/BB12036.balibase-ref-fas 0 / 17604 = 0.0
   example/BB12036.linsi-fas 1556 / 17604 = 8.838900249943195e-2
   example/BB12036.probcons-fas 1602 / 17604 = 9.100204498977506e-2
   example/BB12036.muscle-fas 1784 / 17604 = 0.10134060440808908
   example/BB12036.tcoffee-fas 1826 / 17604 = 0.10372642581231538
   example/BB12036.dialigntx-fas 2044 / 17604 = 0.11610997500568053
   example/BB12036.fftnsi-fas 2096 / 17604 = 0.11906384912519882
   example/BB12036.clustalw-fas 2636 / 17604 = 0.14973869575096568
   example/BB12036.prank-fas 3448 / 17604 = 0.1958645762326744

MAFFT's L-INS-i algorithm is closest, Prank is furthest. Note that although
both Prank and ClustalW are 15-20% different to the reference alignment, this
is not because they are similar to each other, as we determined they were ~15%
different to each other, above. On the other hand, ProbCons and T-COFFEE (9-10%
difference from the reference) are similar to each other (~2% difference
between them). Being able to compare scores between individual alignments as
well as against a reference is one of the advantages of a metric over a
non-metric score.

## Installation from source

MetAl is written in Haskell. 

You need my phytree library first. Neither this nor phyhs are yet on
Hackage, so you`ll have to use git. You also need
[Haskell-Platform](http://haskell.org/platform) installed.

    git clone git://github.com/benb/phytree.git phytree
    cd phytree
    cabal update
    cabal install
    cd ..
    git clone git://github.com/benb/MetAl.git metal
    cd metal
    cabal install
            
## Problems?

Please contact me at <benjamin.blackburne@manchester.ac.uk>

## License

GPL3.

Source code available at http://github.com/benb/MetAl/.

