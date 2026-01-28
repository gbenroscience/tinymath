## tinymath

**tinymath** is a lean, fast and efficient math parser written in native C for maximum speed.
It has, inbuilt into it, the ability to do expression parsing, variables arithmetic, trigonometric computations, functions creation and evaluation, derivatives evaluation and computation and statistics.

### Expression parsing
```
2+2;
4+3*2-5^(3+sin(7));
```

### Variables
```
a11=5.2;b13=3.8;
sin(a11-4*b13);
```

### Functions
```
f(x,y)=3*x^3-5*x^2-4.2*x-13;
g(x)=sin(x+12);
```

### Calculus
```
diff(3*x^2,x);
diff(3*x^2,x,3);
```
The first. evaluates the derivative with respect to $$x$$ and the second computes the value of the derivative at $$x=3$$

### Statistics
A number of statistic functions are supported including:
```
min, max, sum, prod, mean|avg, rng, mrng, std_err, var, std, svar, sstd, median|med, mode, rms 
```

These all take comma separated lists of numbers|variables|functions_to_be_evaluated|or_whatever_resolves_to_a_number as argument.
For examples:
```
a=5;b=3.141;
median(sin(4),8, -19, 24*cos(a-2*b),..)
```