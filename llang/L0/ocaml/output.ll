; ModuleID = 'llang.c'
source_filename = "llang.c"
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-macosx26.0.0"

@.str = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

; Function Attrs: noinline nounwind optnone ssp uwtable(sync)
define i32 @main() #0 {
label_0:
  %22 = add nsw i32 1, 2
  %23 = mul nsw i32 2, %22
  %24 = call i32 (ptr, ...) @printf(ptr noundef @.str, i32 noundef %23)
  ret i32 0
}

declare i32 @printf(ptr noundef, ...) #1
