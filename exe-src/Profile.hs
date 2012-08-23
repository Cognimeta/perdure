{-
Copyright 2010-2012 Cognimeta Inc.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
or implied. See the License for the specific language governing permissions and limitations under the License.
-}

module Profile (
    putCpuTime,
    cpuTimeNF,
    cpuTime
) where

import System.CPUTime
import Control.DeepSeq
import Data.Functor
import Control.Exception

putCpuTime :: String -> IO a -> IO a
putCpuTime s io = cpuTime io >>= \(a, t) -> a <$ (putStrLn $ s ++ " = " ++ show t)

cpuTimeNF :: NFData a => a -> IO (a, Float)
cpuTimeNF f = cpuTime $ evaluate $ (rnf f) `seq` f

cpuTime :: IO a -> IO (a, Float)
cpuTime f = do t0 <- getCPUTime
               a <- f
               t1 <- getCPUTime
               return $ (a, (((fromInteger (t1 - t0)))::Float) / 1e12)


