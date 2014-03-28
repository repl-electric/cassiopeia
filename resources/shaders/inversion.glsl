// Based on "The Inversion Machine" by Kali
// Adapted by Joseph Wilk <joe@josephwilk.net>

uniform float iOvertoneVolume;
uniform float iG;
uniform float iA;
uniform float iD;

const float width=0.08;
const float scale=5.;
const float detail=.0001;
const int circleWarp=1;
const int forward=1;

vec3 lightdir=-vec3(.2,.5,1.);

mat2 rot;

float rand(vec2 co){
  return fract(sin(dot(co.xy ,vec2(12.9898,78.233))) * 43758.5453);
}

float applyMotion(vec3 p) {


  float acceleration=0.0;
  float t=iGlobalTime;
  float dotp=dot(p,p);

  if(forward==1){
    t = -t;
  }

  if(iOvertoneVolume > 0.0){
    acceleration=1.0;
  }else{
    acceleration=0.0;
  }

  //  p.x+=sin(t*40.)*.0007;
  p = p/dotp*scale;
  p = sin(p+vec3(sin(01.+t),t,t*acceleration));
  float d=length(p.yz)-width;
  d = min(d,length(p.xz) - width);
  d = min(d,length(p.xy) - width);

  if(circleWarp==0){
    d=min(d,length(p.xy)-width);
    d=min(d,length(p*p*p)-width*.3);
  }
  return d*dotp/scale;
}

vec3 normal(vec3 p) {
  vec3 e = vec3(0.0,detail,0.0);

  return normalize(vec3(
                        applyMotion(p+e.yxx)-applyMotion(p-e.yxx),
                        applyMotion(p+e.xyx)-applyMotion(p-e.xyx),
                        applyMotion(p+e.xxy)-applyMotion(p-e.xxy)
                        )
                   );
}

float light(in vec3 p, in vec3 dir) {
  vec3 ldir=normalize(lightdir);
  vec3 n=normal(p);
  float sh=1.;
  float diff=max(0.,dot(ldir,-n))+.1*max(0.,dot(normalize(dir),-n));
  vec3 r = reflect(ldir,n);
  float spec=max(0.,dot(dir,-r))*sh;
  return diff+pow(spec,20.)*.7;
}

float raymarch(in vec3 from, in vec3 dir)
{
  vec2 uv = gl_FragCoord.xy / iResolution.xy*2.-1.;
  uv.y*=iResolution.y/iResolution.x;
  float st,d,col,totdist=st=0.;
  vec3 p;
  float ra=rand(uv.xy*iGlobalTime)-1.;
  float ras=max(0.,sign(-.5+rand(vec2(1.3456,.3573)*floor(30.+iGlobalTime*20.))));
  float rab=1.1;
  float rac=0.0;
  float ral=0.0;
  for (int i=0; i<50; i++) {
    p=from+totdist*dir;
    d=applyMotion(p);
    if (d<detail || totdist>3.) break;
    totdist+=d;
    st+=max(0.,.04-d);
  }
  vec2 li=uv*rot;
  float backg=.45*pow(1.5-min(1.,length(li+vec2(0.,-.6))),1.5);
  if (d<detail) {
    col=light(p-detail*dir, dir);
  } else {
    col=backg;
  }
    col+=smoothstep(0.,1.,st)*.8*(.1+rab);
    col+=pow(max(0.,1.-length(p)),8.)*(.5+10.*rab);
    col+=pow(max(0.,1.-length(p)),30.)*50.;
    col = iG * col;
    col = mix(col, backg, 1.0-exp(-.25*pow(totdist,3.)));

  if (rac>.7) col=col*.7+(.3+ra+ral*.5)*mod(uv.y+iGlobalTime*2.,.25);
  col = mix(col, .5+ra+ral*.5, max(0.,3.-iGlobalTime)/3.);
  return col+ra*.03+(ral*.1+ra*.1)*rab;
}

void main(void)
{
  float darkness=5.1;
  float t=iGlobalTime*0.002;
  vec2 uv = gl_FragCoord.xy / iResolution.xy*2.-1.;
  uv.y*=iResolution.y/iResolution.x;
  vec3 from=vec3(0.,0.1,-1.2);
  vec3 dir=normalize(vec3(uv,1.));
  rot=mat2(cos(t),sin(t),-sin(t),cos(t));
  dir.xy=dir.xy*rot;
  float col=raymarch(from,dir);
  col=pow(col,darkness);
  gl_FragColor = vec4(col);
}
