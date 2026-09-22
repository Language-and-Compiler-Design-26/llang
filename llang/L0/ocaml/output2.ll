; ModuleID = 'output.ll'
source_filename = "llang.c"
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-macosx26.0.0"

@.str = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

define i32 @main() {
label_0:
  %0 = add nsw i32 1, 1
  %1 = add nsw i32 1, 2
  %2 = add nsw i32 %1, 3
  %3 = add nsw i32 %2, %0
  %4 = call i32 (ptr, ...) @printf(ptr noundef @.str, i32 noundef %3)
  ret i32 0
}

declare i32 @printf(ptr noundef, ...)
