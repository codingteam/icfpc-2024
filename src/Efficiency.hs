module Efficiency (
    efficiency12
) where

import Data.Function (fix)
import AST

efficiency12 n = 
  fix (
      \v3 v4 ->
        min v4 (
            1 + (
                if (v4 > 2) then (
                    (fix (\v5 v6 v7 ->
                        if v6 == v4 then (
                            v7
                        ) else (
                            (v5 (v6 + 1))
                            (
                                if (v3 v6) > (v6 - 1) then (
                                    if v4 `mod` v6 == 0 then (
                                        (v7 `div` (v3 v6)) * ((v3 v6) - 1)
                                    ) else (
                                        v7
                                    ) 
                                ) else (
                                    v7
                                )
                            )
                        )
                    ) 2) v4
                ) else (
                    v4
                )  
            )
        )
  ) n
