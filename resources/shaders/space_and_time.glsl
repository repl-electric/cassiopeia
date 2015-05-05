//Space and Time by Joseph Wilk <joe@josephwilk.net>

uniform float iOvertoneVolume;
uniform float iGlobalBeatCount;

vec3 hsv2rgb(float h, float s, float v) {
  return mix(vec3(1.), clamp((abs(fract(h+vec3(3.,2.,1.)/3.)*6.-3.)-1.),0.,1.),s)*v;
}

vec3 hsvToRgb(float mixRate, float colorStrength){
  float colorChangeRate = 18.0;
  float time = fract(iGlobalTime/colorChangeRate);
  float movementStart = (mod(iGlobalBeatCount,16) == 0) ? 1.0 : 0.5;
  vec3 x = abs(fract((mod(iGlobalBeatCount,16)-1+time) + vec3(2.,3.,1.)/3.) * 6.-3.) - 1.;
  vec3 c = clamp(x, 0.,1.);
  //c = c*iBeat;
  //c = c * clamp(iBeat, 0.1, 0.4)+0.6;
  return mix(vec3(1.0), c, mixRate) * colorStrength;
}


vec4 lineDistort(vec4 cTextureScreen, vec2 uv1){
  float sCount = 900.;
  float nIntensity=0.1;
  float sIntensity=0.2;
  float noiseEntry = 0.0;
  float accelerator= 1000.0;

  float x = uv1.x * uv1.y * iGlobalTime * accelerator;
  x = mod( x, 13.0 ) * mod( x, 123.0 );
  float dx = mod( x, 0.05 );
  vec3 cResult = cTextureScreen.rgb + cTextureScreen.rgb * clamp( 0.1 + dx * 100.0, 0.0, 1.0 );
  vec2 sc = vec2( sin( uv1.y * sCount ), cos( uv1.y * sCount ) );
  cResult += cTextureScreen.rgb * vec3( sc.x, sc.y, sc.x ) * sIntensity;
  cResult = cTextureScreen.rgb + clamp(nIntensity, noiseEntry,1.0 ) * (cResult - cTextureScreen.rgb);
  return vec4(cResult, cTextureScreen.a);
}

vec2 rotate(vec2 p, float a){
  return vec2(p.x * cos(a) - p.y * sin(a), p.x * sin(a) + p.y * cos(a));
}

float smoothedVolume;

vec4 generateSpaceLights(vec2 uv1){
  vec2 uv = uv1 * 2.0 - 1.0;
  uv.x *= iResolution.x / iResolution.y;

  float v = 0.0;

  vec3 ray = vec3(sin(iGlobalTime * 0.1) * 0.2, cos(iGlobalTime * 0.13) * 0.2, 1.5);
  vec3 dir;
  dir = normalize(vec3(uv, 1.0));

  ray.z += iGlobalTime * 0.001 - 20.0;
  dir.xz = rotate(dir.xz, sin(iGlobalTime * 0.001) * 0.1);
  dir.xy = rotate(dir.xy, iGlobalTime * 0.01);

  #define STEPS 8

  smoothedVolume += (iOvertoneVolume  - smoothedVolume) * 0.1;

  float inc = smoothedVolume / float(STEPS);
  if (iOvertoneVolume<=0.01){
    inc = 0;
  }
  else{
    inc = clamp(inc, 0.2,0.8);
  }

  vec3 acc = vec3(0.0);

  for(int i = 0; i < STEPS; i ++){
    vec3 p = ray * 0.1;

    for(int i = 0; i < 14; i ++){
      p = abs(p) / dot(p, p) * 2.0 - 1.0;
    }
    float it = 0.001 * length(p * p);
    v += it;

    acc += sqrt(it) * texture2D(iChannel1, ray.xy * 0.1 + ray.z * 0.1).xyz;
    ray += dir * inc;
  }

  float br = pow(v * 4.0, 3.0) * 0.1;
  vec3 col = pow(acc * 0.5, vec3(1.2)) + br;
  return vec4(col, 1.0);
}

float normpdf(in float x, in float sigma)
{
  return 0.39894*exp(-0.5*x*x/(sigma*sigma))/sigma;
}



float time = iGlobalTime;
vec2 mouse = iMouse.xy/iResolution.xy;
vec2 resolution = iResolution.xy;

const float PI = 3.141592653589;

float cap(vec2 a, vec2 b) {
  vec2 abd = vec2(a.x*b.x+a.y*b.y, a.y*b.x-a.x*b.y);
  float y_x = abd.y/(abd.x-1.);

  return atan(-y_x)-y_x/(1.+y_x*y_x)+PI/2.;
}

float cap1(float p) {
  p = max(min(p,1.),-1.);
  return asin(p)+p*sqrt(1.-p*p)+PI/2.;
}

float ebok(vec2 p, vec2 a, vec2 b) {
  vec2 an = vec2(a.y,-a.x);
  vec2 bn = vec2(b.y,-b.x);

  float surface;
  if (dot(normalize(an),normalize(bn))>.9999) {
    // This is neccessary to remove dot crawl around corners
    surface = 0.;
  } else if (dot(p,p) < .99) {
    float pa = dot(p,a);
    float ra = -pa+sqrt(pa*pa-dot(p,p)+1.);
    vec2 pac = ra*a;

    float pb = dot(p,b);
    float rb = -pb+sqrt(pb*pb-dot(p,p)+1.);
    vec2 pbc = rb*b;

    surface = cap(p+pac,p+pbc)+(pac.x*pbc.y-pac.y*pbc.x)*.5;
  } else {
    float d1 = dot(an,p);
    float d2 = -dot(bn,p);
    float sda = step(dot(p,a),0.);
    float sdb = step(dot(p,b),0.);
    surface = PI*(sda+sdb-sda*sdb) - cap1(-d1)*sda - cap1(-d2)*sdb;

  }
  return surface;
}

float handleCorner(vec2 p, vec2 a, vec2 b, vec2 c) {
  vec2 ba = normalize(a-b);
  vec2 bc = normalize(c-b);
  float h = dot(a-p,vec2(ba.y,-ba.x));
  return ebok(p-b, bc, ba) - cap1(h);
}

float bokehtria(vec2 p, vec2 a, vec2 b, vec2 c) {
  vec2 mi = min(min(a,b),c)-1.;
  vec2 ma = max(max(a,b),c)+1.;
  return (a.x-b.x)*(a.y-c.y)<(a.y-b.y)*(a.x-c.x)||p.x<mi.x||p.y<mi.y||p.x>ma.x||p.y>ma.y ? 0. :  handleCorner(p,a,b,c) + handleCorner(p,b,c,a) + handleCorner(p,c,a,b) + PI;
}

float bokehsquare(vec2 p, vec2 a, vec2 b, vec2 c, vec2 d, float scale) {
  p *= scale; a *= scale; b *= scale; c *= scale; d *= scale;
  vec2 mi = min(min(a,b),min(c,d))-1.;
  vec2 ma = max(max(a,b),max(c,d))+1.;
  return (a.x-b.x)*(a.y-c.y)<(a.y-b.y)*(a.x-c.x)||p.x<mi.x||p.y<mi.y||p.x>ma.x||p.y>ma.y ? 0. :  handleCorner(p,a,b,c) + handleCorner(p,b,c,d) + handleCorner(p,c,d,a) + handleCorner(p,d,a,b) + PI;
}

vec2 project(vec3 v) {
  return v.xy/(v.z+14.);
}

vec4 shade(vec3 v, float f) {
  float highlight = pow(f*.5+.5,100.);
  return vec4(pow(f*.5+.5,10.)*v*1.5*(1.-highlight)+highlight,1.)/PI;
}



// Bluring fn froms:
// Bokeh disc.
// by David Hoskins.
// License Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License.
#define PI 3.141596

// This is (3.-sqrt(5.0))*PI radians, which doesn't precompiled for some reason.
// The compiler is a dunce I tells-ya!!
#define GOLDEN_ANGLE 2.39996323

#define NUMBER 150.0

#define ITERATIONS (GOLDEN_ANGLE * NUMBER)

//-------------------------------------------------------------------------------------------
// This creates the 2D offset for the next point.
// (r-1.0) is the equivalent to sqrt(0, 1, 2, 3...)
vec2 Sample(in float theta, inout float r){
  r += 1.0 / r;
  return (r-1.0) * vec2(cos(theta), sin(theta));
}

vec3 Bokeh(sampler2D tex, vec2 uv, float radius, float amount){
  vec3 acc = vec3(0.0);
  vec3 div = vec3(0.0);
  vec2 pixel = vec2(iResolution.y/iResolution.x, 1.0) * radius * .002;
  float r = 1.0;
  for (float j = 0.0; j < ITERATIONS; j += GOLDEN_ANGLE){
      vec2 idx = uv + pixel * Sample(j, r);
      idx.y=0.25;
      vec3 col = texture2D(tex, idx).xyz;
      col = col * col * 1.4; // ...contrast it for better highlights
      vec3 bokeh = vec3(5.0) + pow(col, vec3(9.0)) * amount;
      acc += col * bokeh;
      div += bokeh;
  }
  return acc / div;
}

vec4 blur(vec2 uv){
  vec2 uv2 = gl_FragCoord.xy / iResolution.xy;
  float r = 0.3;
  float a = 0.0;
  uv2 = vec2(iResolution.y/uv.y, 0.25);

  float d = clamp(uv.y/8, 0.0, iResolution.y);
  uv2 = vec2(pow(d, 1.0), 0.25);
  return vec4(Bokeh(iChannel0, uv2, r, a), 1.0);
}

vec4 textureCutout(vec4 w, vec4 tex){
  vec4 logoAlpha = tex.aaaa;
  vec4 negAlpha = logoAlpha * vec4(-1.,-1.,-1.,0.) + vec4(1.,1.,1.,0.);
  w = negAlpha - (w + logoAlpha);
  return w;
}

vec3 mod289(vec3 x){
  return x - floor(x * (1.0 / 289.0)) * 289.0;
}

vec2 mod289(vec2 x) {
  return x - floor(x * (1.0 / 289.0)) * 289.0;
}

vec3 permute(vec3 x){
  return mod289(((x*34.0)+1.0)*x);
}

float snoise(vec2 v){
  const vec4 C = vec4(0.211324865405187, 0.366025403784439, -0.577350269189626, 0.024390243902439);
  vec2 i = floor(v + dot(v, C.yy));
  vec2 x0 = v - i + dot(i, C.xx);

  vec2 i1;
  i1 = (x0.x > x0.y) ? vec2(1.0, 0.0) : vec2(0.0, 1.0);
  vec4 x12 = x0.xyxy + C.xxzz;
  x12.xy -= i1;

  i = mod289(i); // Avoid truncation effects in permutation
  vec3 p = permute(permute(i.y + vec3(0.0, i1.y, 1.0 )) + i.x + vec3(0.0, i1.x, 1.0 ));
  vec3 m = max(0.5 - vec3(dot(x0,x0), dot(x12.xy,x12.xy), dot(x12.zw,x12.zw)), 0.0);
  m = m*m;
  m = m*m;

  vec3 x = 2.0 * fract(p * C.www) - 1.0;
  vec3 h = abs(x) - 0.5;
  vec3 ox = floor(x + 0.5);
  vec3 a0 = x - ox;

  m *= 1.79284291400159 - 0.85373472095314 * (a0*a0 + h*h);

  vec3 g;
  g.x  = a0.x  * x0.x  + h.x  * x0.y;
  g.yz = a0.yz * x12.xz + h.yz * x12.yw;
  return 130.0 * dot(m, g);
}

vec2 warpedCords(vec2 p){
  float distortion=0.0001;
  float distortion2=0.5;
  float speed=0.02;
  float rollSpeed=0.0;
  float ty = iGlobalTime* speed;
  float yt = p.y - ty;
  float offset = snoise(vec2(yt*3.0,0.0))*0.2;
  offset = pow(offset*distortion, 3.0)/distortion ;
  offset += snoise(vec2(yt*50.0,0.0))*distortion2*0.001;
  vec2 o = vec2(fract(p.x + offset), fract(p.y-iGlobalTime*rollSpeed));
  return o;
}


void main(void){
  vec2 uv = gl_FragCoord.xy / iResolution.x;
  vec4 r = generateSpaceLights(uv) + vec4(hsvToRgb(0.0,0.0),1.0);

  float time = 0.0;
  float space = 0.0;
  float blurWeight = 0.0;

  uv.y += 0.0;
  //  uv.x= uv.x*1.0;
  //  uv.y= uv.y*1;

  //r = r+blur(uv);
  //  vec4 poop = vec4(texture2D(iChannel0, uv).xyz, 1.0);
  vec4 blurR = blur(uv);

  if(blurWeight > 0.0){
    r += blurR;}
  else{
    //
  //    float d = clamp(uv.y/8, 0.0, iResolution.y);
  //r += texture2D(iChannel0, vec2(pow(d, 1.0), 0.25));
  }

  r = r - time*(textureCutout(vec4(0.0,0.0,0.0,1.0), texture2D(iChannel2, warpedCords(uv))));
  r = r - space*(textureCutout(vec4(0.0,0.0,0.0,1.0),texture2D(iChannel3, warpedCords(uv))));


  gl_FragColor = lineDistort(r, uv);
}
