import json,os,urllib.request as u
V=[100,320,330,500,900,0]
A=['SSSSX]]?XNISSSSgXX]l]]gq~~~~SSSS','?IIIIXSSI]]]IS]]IXX]ISX]ISSS?III']
DR=[0,0,(-33,-31,-18,-14,14,18,31,33),(-17,-15,15,17),(-16,16,-1,1),0,0]
DR[5]=DR[6]=DR[3]+DR[4]
B=[0]*128;S=1;C=0;G=0;L={1:4,-1:116};N=[0];Y={0:2,7:1,112:8,119:4};E=-1
I=10**6;M=10**5;Q=99999
def w(p,q):
 r=q^112 if p<0 else q;f=r&7;return (V[abs(p)-1]+ord(A[abs(p)!=1][(r>>4)*4+min(f,7-f)])-83)*(p//abs(p))
def pf(f):
 global B,S,C,G,L,E
 y=f.split();B=[0]*128;G=0;s=112
 for ch in y[0]:
  if ch=="/":s-=24
  elif ch.isdigit():s+=int(ch)
  else:
   c="PNBRQKpnbrqk".find(ch);c=c+1 if c<6 else 5-c
   B[s]=c;G+=w(c,s);s+=1
 L[1]=B.index(6);L[-1]=B.index(-6)
 S=1-2*(y[1]=="b")
 C=sum(1<<"KQkq".index(c) for c in y[2] if c in"KQkq")
 E=ord(y[3][0])-97+(int(y[3][1])-1)*16 if y[3]!="-" else -1
def at(q,y):
 for t in (q-16*y-1,q-16*y+1):
  if not t&136 and B[t]==y:return 1
 for d in DR[2]+DR[6]:
  t=q+d
  if not t&136 and B[t]==(2 if d in DR[2] else 6)*y:return 1
 for d in DR[5]:
  t=q+d
  while not t&136:
   z=B[t]
   if z and z*y in(5,4 if d in DR[4] else 3):return 1
   if z:break
   t+=d
 return 0
def gn(c):
 ms=[]
 for f in range(128):
  if f&136:continue
  pc=B[f]
  if not pc or pc*S<0:continue
  t=abs(pc)
  if t==1:
   d=16*S;r=f>>4;pr=r==(7+5*S)//2
   for to in (f+d,f+d-1,f+d+1):
    if to&136:continue
    q=B[to]
    if to==f+d and q:continue
    if to!=f+d and (not q and to!=E or q*S>0):continue
    if pr:
     for p in (5,4,3,2):ms.append(f|(to<<7)|(p<<14))
    else:
     ms.append(f|(to<<7))
     if to==f+d and not c and r==(7-5*S)//2 and not B[f+2*d]:ms.append(f|((f+2*d)<<7))
  else:
   for d in DR[t]:
    to=f+d
    while not to&136:
     q=B[to]
     if not q and not c or q*S<0:ms.append(f|(to<<7))
     if q or t==2 or t==6:break
     to+=d
 if not c:
  k=L[S]
  for d in(1,-1):
   b=1<<(2*(S<0)+(d<0));t=k+2*d;r=k-4+7*(d>0)
   if C&b and B[r]==4*S and not(B[k+d]or B[t])and(d>0 or not B[k+3*d])and all(not at(x,-S)for x in(k,k+d,t)):ms.append(k|(t<<7))
 return ms
def rs(o):
 global S,C,G,E
 B[:]=o[0];S,C,G,E=o[1:5];L.update(o[5])
def mk(m):
 global S,C,G,E
 o=B[:],S,C,G,E,dict(L)
 f=m&127;t=(m>>7)&127;p=(m>>14)&7;pc=B[f];cap=B[t]
 G-=w(pc,f);B[f]=0
 if abs(pc)==1 and t==E:G-=w(B[t-16*S],t-16*S);B[t-16*S]=0
 if cap:G-=w(cap,t);B[t]=0
 npc=p*S if p else pc;B[t]=npc;G+=w(npc,t)
 C&=~(Y.get(f,0)|Y.get(t,0))
 if abs(pc)==6:
  L[S]=t
  if t-f==2:rf,rt=f+3,t-1
  elif f-t==2:rf,rt=f-4,t+1
  else:rf=-1
  if rf>=0:B[rf]=0;B[rt]=4*S;G-=w(4*S,rf);G+=w(4*S,rt)
  C&=~(3 if S>0 else 12)
 E=f+16*S if abs(pc)==1 and abs(t-f)==32 else -1
 S=-S
 return o
def od(ms,pv):ms.sort(key=lambda m:999 if m==pv else 10*abs(B[m>>7&127])+900*(m>>14>0),reverse=True)
def sr(d,a,b):
 N[0]+=1
 if N[0]>Q:raise StopIteration
 ck=at(L[S],-S);best=-I
 if d<=0 and not ck:
  s=S*G
  if s>=b:return b
  if s>a:a=s
  best=a
 ms=gn(0 if d>0 or ck else 1);od(ms,0)
 for m in ms:
  t=(m>>7)&127
  if d<=0 and not ck and not m>>14 and s+V[abs(B[t])-1]+99<a:continue
  o=mk(m)
  if at(L[-S],S):rs(o);continue
  v=-sr(d-1,-b,-a);rs(o)
  if v>best:best=v
  if v>a:a=v
  if a>=b:break
 if best==-I:return -M if ck else 0
 return best
def rt(g):
 pf(g["fen"]);ms=[]
 for m in gn(0):
  o=mk(m)
  if not at(L[-S],S):ms.append(m)
  rs(o)
 best=ms[0];pv=0;N[0]=0
 for dep in range(12):
  try:
   a=-I;b=I;bv=-I;bm=ms[0];od(ms,pv)
   for m in ms:
    o=mk(m);v=-sr(dep,-b,-a);rs(o)
    if v>bv:bv=v;bm=m
    if v>a:a=v
   best=bm;pv=bm
  except StopIteration:pf(g["fen"]);break
 b=best;return [chr(97+(b&7))+str(1+(b>>4&7))+chr(97+((b>>7)&7))+str(1+(b>>11&7))+("nbrq"[(b>>14)-2] if b>>14 else "")]+g["legal_moves"]
def po(p,b):
 try:return json.load(u.urlopen(os.getenv("MAKEFUN_ENV_URL")+p,json.dumps(b).encode()))
 except:return {"error":1}
def rn():
 while 1:
  g=po("/game",{})
  if g.get("status")=="done"or"error"in g:break
  while g.get("status")=="your_move":
   try:c=rt(g)
   except Exception:c=g["legal_moves"]
   for v in c:
    r=po("/move",{"game":g["game"],"move":v})
    if "error"not in r:g=r;break
   else:po("/resign",{"game":g["game"]});break
rn()
