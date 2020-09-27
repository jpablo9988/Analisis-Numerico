import numpy as np
def ecuaciones(ec1,ec2,x0,tol):
  x=x0
  error=1
  n=0
  while error > tol:
    dx= -np.linalg.solve(ec1(*x),ec2(*x))
    error = np.linalg.norm(dx)/np.linalg.norm(x)
    x+=dx;
    n+1
  print(n)
  return x

ec1 = lambda x1,x2: [x1+(x1+x2)*2.7**(x1+x2)-3,x1+(2*x1+x2)*2.7**(2*x1+x2)] ec2 = lambda x1,x2: [[2.7**(x1+x2)+2.7**(x1+x2)*(x1+x2)+1,2.7*(x2+x1)+2.7**(x2+x1)*(x2+x1)],[2*2.7**(2*x1+x2)+2*2.7**(2*x1+x2)*(2*x1+x2)+1,2.7**(x2+2*x1)+2.7**(x2+2*x1)*(x2+2*x1)]]
ecuaciones(ec1,ec2,[1,2],1e-8)