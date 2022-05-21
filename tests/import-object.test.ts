import { readFileSync } from "fs";
import { gcd, lcm, factorial } from "../stdlib/math";

enum Type { Num, Bool, None, Ellipsis, FLOAT }

function stringify(typ: Type, arg: any): string {
  switch (typ) {
    case Type.Num:
      return (arg as number).toString();
    case Type.Bool:
      return (arg as boolean) ? "True" : "False";
    case Type.None:
      return "None";
    case Type.Ellipsis:
      return "Ellipsis";
  }
}

function print(typ?: Type, arg?: any): any {
  if(typ!== undefined){
    importObject.output += stringify(typ, arg)+" ";
  } else{
    if (importObject.output.length>0){
      importObject.output = importObject.output.substring(0, importObject.output.length-1)
    }
    importObject.output += "\n";
  }
  return 0;
}

function assert_not_none(arg: any) : any {
  if (arg === 0)
    throw new Error("RUNTIME ERROR: cannot perform operation on none");
  return arg;
}

export async function addLibs() {
  const bytes = readFileSync("build/memory.wasm");
  const memory = new WebAssembly.Memory({initial:10, maximum:100});
  const memoryModule = await WebAssembly.instantiate(bytes, { js: { mem: memory } })
  importObject.libmemory = memoryModule.instance.exports,
  importObject.memory_values = memory;
  importObject.js = {memory};
  return importObject;
}

export const importObject : any = {
  imports: {
    // we typically define print to mean logging to the console. To make testing
    // the compiler easier, we define print so it logs to a string object.
    //  We can then examine output to see what would have been printed in the
    //  console.
    assert_not_none: (arg: any) => assert_not_none(arg),
    print_num: (arg: number) => print(Type.Num, arg),
    print_bool: (arg: number) => print(Type.Bool, arg),
    print_none: (arg: number) => print(Type.None, arg),
    print_newline: (arg: number) => print(undefined, arg),
    print_ellipsis: (arg: number) => print(Type.Ellipsis, arg),
    print_float: (arg: number) => print(Type.FLOAT, arg),
    int: (arg: any) => arg,
    bool: (arg: any) => arg !== 0,
    abs: Math.abs,
    min: Math.min,
    max: Math.max,
    pow: Math.pow,
    gcd: gcd,
    lcm: lcm,
    factorial: factorial,
  },

  output: "",
};
