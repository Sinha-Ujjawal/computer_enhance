/* ========================================================================

   (C) Copyright 2023 by Molly Rocket, Inc., All Rights Reserved.
   
   This software is provided 'as-is', without any express or implied
   warranty. In no event will the authors be held liable for any damages
   arising from the use of this software.
   
   Please see https://computerenhance.com for more information
   
   ======================================================================== */

/* ========================================================================
   LISTING 200
   ======================================================================== */

#if defined(__clang__) || defined(__GNUC__)
#define PretendToWriteF64(Variable) asm volatile ("vpxor %0,%0,%0" : "=x"(Variable))
#define ForceCompilerToSetF64(Variable, Value) asm volatile ("vmovsd %1,%0" : "=x"(Variable) : "g"(Value))
#define PretendToRead(Variable) asm volatile ("" : : "x"(Variable))
#else
#define PretendToWriteF64(Variable) (void)(Variable)
#define ForceCompilerToSetF64(Variable, Value) (Variable) = (Value)
#define PretendToRead(Variable) (void)(Variable)
#pragma message("WARNING: Dead code macros are not available on this compiler.")
#endif
