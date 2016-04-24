; ModuleID = 'module'

@printf_format = private constant [5 x i8] c"%lf\0A\00"
@scanf_format = private constant [4 x i8] c"%lf\00"

declare i32 @printf(i8*, ...)

declare i32 @scanf(i8*, ...)

define void @print(double %value) {
entry:
  %value1 = alloca double
  store double %value, double* %value1
  %0 = load double, double* %value1
  %1 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @printf_format, i32 0, i32 0), double %0)
  ret void
}

define double @input() {
entry:
  %input_float = alloca double
  %0 = getelementptr inbounds double, double* %input_float, i32 0
  %1 = call i32 (i8*, ...) @scanf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @scanf_format, i32 0, i32 0), double* %0)
  %floattmp = load double, double* %input_float
  ret double %floattmp
}

define double @fib(double %i) {
entry:
  %current = alloca double
  %hi = alloca double
  %lo = alloca double
  %i1 = alloca double
  store double %i, double* %i1
  store double 0.000000e+00, double* %lo
  store double 1.000000e+00, double* %hi
  store double 0.000000e+00, double* %current
  br label %loopheader

loopheader:                                       ; preds = %loop, %entry
  %0 = load double, double* %i1
  %1 = load double, double* %current
  %cmpulttmp = fcmp ult double %1, %0
  br i1 %cmpulttmp, label %loop, label %loopexit

loop:                                             ; preds = %loopheader
  %2 = load double, double* %current
  %addtmp = fadd double %2, 1.000000e+00
  store double %addtmp, double* %current
  %3 = load double, double* %hi
  %4 = load double, double* %lo
  %addtmp2 = fadd double %4, %3
  store double %addtmp2, double* %hi
  %5 = load double, double* %lo
  %6 = load double, double* %hi
  %subtmp = fsub double %6, %5
  store double %subtmp, double* %lo
  br label %loopheader

loopexit:                                         ; preds = %loopheader
  %7 = load double, double* %lo
  ret double %7
}

define void @main() {
entry:
  %n = alloca double
  %0 = call double @input()
  store double %0, double* %n
  %1 = load double, double* %n
  %2 = call double @fib(double %1)
  call void @print(double %2)
  ret void
}
