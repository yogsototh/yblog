a=27;b=79;c=C(-2.0,-1.0);d=C(1.0,1.0);e=C(-2.501,-1.003)
newtype C = C (Double,Double) deriving (Show,Eq)
instance Num C where C(x,y)*C(z,t)=C(z*x-y*t,y*z+x*t);C(x,y)+C(z,t)=C(x+z,y+t);abs(C(x,y))=C(sqrt(x*x+y*y),0.0)
r(C(x,y))=x;i(C(x,y))=y
f c z 0=0;f c z n=if(r(abs(z))>2)then n else f c ((z*z)+c) (n-1)
h j k = map (\z->(f (C z) (C(0,0)) 32,(fst z>l - q/2))) [(x,y)|y<-[p,(p+((o-p)/a))..o],x<-[m,(m + q)..l]] where o=i k;p=i j;m=r j;l=r k;q=(l-m)/b
u j k = concat $ map v $ h j k where v (i,p)=(" .,`'°\":;-+oO0123456789=!%*§&$@#"!!i):rst p;rst True="\n";rst False=""
main = putStrLn $ im 0 where cl n (C (x,y))=let cs=(1.1**n-1) in C ((x+cs*(r e))/cs+1,(y+cs*(i e))/cs+1);bl n=cl n c;tr n=cl n d;im n=u (bl n) (tr n)++"\x1b[H\x1b[25A"++im (n+1)
