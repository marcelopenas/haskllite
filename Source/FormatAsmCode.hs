module Source.FormatAsmCode (formatAsmCode) where

formatAsmCode :: String -> String
formatAsmCode asmCode =
  unlines
    [ "section .data",
      "  format_out: db \"%d\", 10, 0 ; format printf",
      "  format_in: db \"%d\", 0 ; format scanf",
      "  scan_int: dd 0; 32-bits integer",
      "",
      "section .text",
      "",
      "  extern printf",
      "  extern scanf",
      "  global _start",
      "",
      "_start:",
      "  push ebp ; store EBP",
      "  mov ebp, esp ; clear stack",
      "  ; Code start"
    ]
    ++ asmCode
    ++ unlines
      [ "",
        "  ; Code end",
        "  mov esp, ebp ; re-establish stack",
        "  pop ebp",
        "",
        "; exit",
        "  mov eax, 1",
        "  xor ebx, ebx",
        "  int 0x80"
      ]
