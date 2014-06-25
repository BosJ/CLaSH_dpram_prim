module Dpram where

import CLaSH.Prelude

-- True Dual-Port RAM with a Single Clock
--
-- http://www.altera.com/support/examples/vhdl/vhd-true-dual-port-ram-sclk.html
-- http://www.altera.com/literature/hb/cyclone-iv/cyiv-51003.pdf
--
-- Evaluate example: sampleN 5 $ expectedOutput (topEntity testInput)
--
-- In the VHDL; only the length of the 'emptyVec' vector is actually used, 
-- the ram is not (automatically) initialized with these values. Also,
-- only (VHDL) resolved types can be put into ram e.g. Int is not possible.

topEntity :: Signal ((Unsigned 8), (Unsigned 8), (Unsigned 8), (Unsigned 8), Bool, Bool) -> Signal (Unsigned 8)
topEntity input = out where
    (da,db,ra,rb,ea,eb) = unpack input
    (out, _) = trueDpram emptyVec da db ra rb ea eb
    emptyVec = 9:>8:>7:>6:>5:>4:>3:>2:>1:>0:>Nil 

expectedOutput :: Signal (Unsigned 8) -> Signal Bool
expectedOutput = outputVerifier $(v [0 :: (Unsigned 8),0,0,0,5,5])

testInput :: Signal ((Unsigned 8), (Unsigned 8), (Unsigned 8), (Unsigned 8), Bool, Bool)     
testInput = stimuliGenerator $(v [
  ( 0 :: (Unsigned 8), 0 :: (Unsigned 8), 0 :: (Unsigned 8), 1 :: (Unsigned 8), True :: Bool, True :: Bool),
  ( 0, 5, 0, 2, True,  True  ),   -- write addr 0 with 0, write addr 2 with 5
  ( 0, 5, 0, 2, True,  True  ),   -- write addr 0 with 0, write addr 2 with 5
  ( 0, 0, 2, 0, False, False ),   -- read addr 2, read addr 0
  ( 0, 0, 2, 0, False, False ),   -- read addr 2, read addr 0
  ( 0, 0, 2, 0, False, False ) ]) -- read addr 2, read addr 0

{-# NOINLINE trueDpram #-}
trueDpram :: (Num a, KnownNat n, KnownNat m) 
    => Vec n a               -- ^ init vector
    -> Signal a              -- ^ data_a
    -> Signal a              -- ^ data_b
    -> Signal (Unsigned m)   -- ^ addr_a
    -> Signal (Unsigned m)   -- ^ addr_b
    -> Signal Bool           -- ^ we_a
    -> Signal Bool           -- ^ we_b
    -> (Signal a, Signal a)
trueDpram binit din_a din_b addr_a addr_b we_a we_b = 
  (bram' <^> (binit,(0,0))) (din_a,din_b,addr_a,addr_b,we_a,we_b)
    where
      bram' (ram,(oa,ob)) (da,db,addra,addrb,wea,web) = ((ram',out),(oa,ob))
        where
          ram' | wea && web             = vreplace (vreplace ram addra da) addrb db
               | wea && (not web)       = vreplace ram addra da
               | (not wea) && web       = vreplace ram addrb db
               | otherwise              = ram
          out  | (not wea) && (not web) = ((ram!addra), (ram!addrb))
               | (not wea) && web       = ((ram!addra), ob)
               | (wea) && (not web)     = (oa, (ram!addrb))
               | otherwise              = (oa,ob)


