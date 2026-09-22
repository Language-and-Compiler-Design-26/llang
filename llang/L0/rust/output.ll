; ModuleID = 'demo'
source_filename = "demo"

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

define i32 @main() {
label_0:
  %printf_call = call i32 (ptr, ...) @printf(ptr @fmt, i32 2)
  ret i32 0
}

declare i32 @printf(ptr, ...)
