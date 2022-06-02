import { assertPrint, assertFail, assertTCFail, assertTC } from "./asserts.test";
import { NUM, BOOL,FLOAT, NONE, CLASS } from "./helpers.test"

describe("builtin tests", () => {
    // 1
    assertPrint("print-1-arg", `print(1)`, [`1`]);
    // 2
    assertPrint("print-2-args", `print(1,2)`, [`1 2`]);
    // 3
    assertPrint(`print-null`, `print()`,[``]);

    assertPrint("print-5-args", `print(1,2,1,2,1)`, [`1 2 1 2 1`]);


    // 8
    assertPrint("ellipsis", `print(...)`, [`Ellipsis`]
      );
    // 9
    assertTC("float-init", `x : float = 3.2` , NONE);
    // 10
    assertTC("basic-float", 
        `x : float = 3.2
        x` , 
        FLOAT);
    // 11 
    assertPrint("print-float", `print(1.25)`, [`1.25`]);
    // 12
    assertPrint("float-add", `print(1.25+2.25)`, [`3.5`]);
    // 13
    assertPrint("float-lt", `print(1.3>4.3)`, [`False`]);
    // 14
    assertPrint("float-uniop", `print(-2.4)`, [`-2.4000000953674316`]);
    // 15
    assertPrint("float-variable", `x:float=2.4\nprint(x)`, [`2.4000000953674316`]);

    // 16
    assertPrint("float-2-variables", `x:float=2.4\nc:float=2.5\nc=x\nprint(-c)`, [`-2.4000000953674316`]);

    // import / math
  });
  
