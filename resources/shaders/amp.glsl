//AMP
//Created by Joseph Wilk <joe@josephwilk.net>
uniform float iMixRate;
uniform float iRColor;
uniform float iA;
uniform float iRes;
uniform float iSpace;
uniform float iExpand;
uniform float iYinYan;
uniform float iOvertoneVolume;
uniform float iBeat;
uniform float iBeatCount;

const float scale=50.5;
const float detail=50.5;
const float width=0.001;

const int ampMode=1;
const int smoothWave=0;
const float waveReducer=.03;
const int lightOn=0;

vec3 lightdir = vec3(.1,.0,0.);

vec3 mod289(vec3 x) {
  return x - floor(x * (1.0 / 289.0)) * 289.0;
}

vec2 mod289(vec2 x) {
  return x - floor(x * (1.0 / 289.0)) * 289.0;
}

vec3 permute(vec3 x) {
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

float motion(vec3 p) {
  float acceleration = 10.0;
  float movementReducer = 10.0;

  float t=iGlobalTime;
  float dotp=dot(p,p);

   p = p/dotp*scale;
   p = sin(p+vec3(1,t/movementReducer,t*acceleration/movementReducer));
   float d=length(p.yz)-width;
   d = min(d,length(p.xz) - width);
   d = min(d,length(p.xy) - width);
   d = min(d,length(p*p*p)-width*.3);
   return d*dotp/scale;
}

vec3 normal(vec3 p) {
  vec3 e = vec3(0.0, detail, 0.0);

  return normalize(vec3(motion(p+e.yxx)-motion(p-e.yxx),
                        motion(p+e.xyx)-motion(p-e.xyx),
                        motion(p+e.xxy)-motion(p-e.xxy)));
}

vec4 textureCutout(vec4 w, vec4 tex){
  vec4 logoAlpha = tex.aaaa;
  vec4 negAlpha = logoAlpha * vec4(-1.,-1.,-1.,0.) + vec4(1.,1.,1.,0.);
  w = negAlpha - (1-w + logoAlpha);
  return w;
}

float light(in vec3 p, in vec3 dir) {
  vec3 ldir=normalize(lightdir);
  vec3 n=normal(p);
  float sh=0.99;
  float diff=max(0.,dot(ldir,-n))+.1*max(0.,dot(normalize(dir),-n));
  vec3 r = reflect(ldir,n);
  float spec=max(0.,dot(dir,-r))*sh;
  return diff+pow(spec,20.)*.2;
}

float smoothbump(float center, float width, float x, float orien) {
  float lineWidth = width/2.0;
  float centerPlus = center+lineWidth;
  float centerMinus = center-lineWidth;
  float c;

  if(orien > 0.0){
    x = orien-x;
  }

  if(iYinYan > 0.0){
    c = iYinYan*smoothstep(centerMinus, center, x);
  }
  else{
    c = smoothstep(centerMinus, center, x) * 1-smoothstep(center, centerPlus, x);
  }
  return c;
}

vec3 hsv2rgb(float mixRate,float v) {
  float colorChangeRate = 18.0;
  float time = fract(iGlobalTime/colorChangeRate);
  float movementStart = (iBeatCount == 0) ? 1.0 : 0.5;
  vec3 x = abs(fract((iBeatCount-1+time) + vec3(2.,3.,1.)/3.) * 6.-3.) - 1.;
  vec3 c = clamp(x, 0.,1.);
  //c = c*iBeat;
  //c = c * clamp(iBeat, 0.1, 0.4)+0.6;
  return mix(vec3(1.0), c, 1.0)*v;
}

vec4 generateWave(vec2 uv, float yOffset, float orien, float waveReductionFactor){
  float centerOffset=1.4;

  vec4 wa = texture2D(iChannel0, vec2(uv.x, iRes*2.6));
  float wave = waveReductionFactor*wa.x;

  if(smoothWave==0){
    wave = smoothbump(centerOffset,(6/iResolution.y), wave+uv.y-0.9-yOffset, orien); //0.5
    //wave = (-1.0 * wave)+0.5;
  }
  vec3  wc     = wave * hsv2rgb(iMixRate,iRColor);

  float zf     = -0.05;
  vec2  uv2    = (1.0+zf)*uv-(zf/2.0,zf/2.0);
  vec3  pc     = iSpace*texture2D(iChannel1, uv2).rgb;

  return vec4(vec3(wc+pc), 1.0);
}

float rand(vec2 co){
  return fract(sin(dot(co.xy ,vec2(12.9898,78.233))) * 43758.5453);
}

vec4 generateSnow(vec2 p, vec4 tex){
  float size = 8.0;
  vec4 color = tex;
  float amount=0.9;
  float xs = floor(gl_FragCoord.x / size);
  float ys = floor(gl_FragCoord.y / size);
  vec4 snow = vec4(rand(vec2(xs * iGlobalTime,ys * iGlobalTime))*amount);
  return snow;
}

vec2 warpedCords(vec2 p){
  float distortion=0.0001;
  float distortion2=0.5;
  float speed=0.05;
  float rollSpeed=0.0;
  float ty = iGlobalTime* speed;
  float yt = p.y - ty;
  float offset = snoise(vec2(yt*3.0,0.0))*0.2;
  offset = pow(offset*distortion, 3.0)/distortion ;
  offset += snoise(vec2(yt*50.0,0.0))*distortion2*0.001;

  vec2 o = vec2(fract(p.x + offset),-fract(p.y-iGlobalTime*rollSpeed));
  return o;
}

void main(void)
{

  float smoothedVolume;
  smoothedVolume += (iOvertoneVolume  - smoothedVolume) * 0.1;

  float space = 0.1;
  float res = 0.75;

  vec2 uv = gl_FragCoord.xy / iResolution.xy;
  vec2 uv1 = gl_FragCoord.xy / iResolution.xy;

  //Circle background
  vec2 pos = mod(gl_FragCoord.xy, vec2(5.0)) - vec2(0.0);
  float dist_squared = dot(pos, pos);
  vec4 c = (dist_squared < 0.6) ? vec4(.0, .0, .0, 0.0): vec4(1.0, 1.0, 1.0, 1.0);

  float radius = 0.29;
  float scale = 32.1;
  float noCircles = 1.0;

  uv.x = (radius * cos(uv.x * (6.2 * noCircles)));
  uv.y = (radius * sin(uv.y * (3.6 * noCircles)));
  uv.x = uv.x + 0.90 + (clamp(iBeat,0.01,0.02));
  uv.y = uv.y * 0.9;

  vec4 w;
  vec4 wave1;
  vec4 wave2;
  vec4 wave3;
  vec4 wave4;

  if(ampMode==1){
    wave1  = generateWave(uv, 0.0, uv.x,  waveReducer);
    wave2  = generateWave(uv, 0.1, uv.x,  waveReducer);
    wave3  = generateWave(uv, 0.05, uv.x, waveReducer);

    w = c * mix(wave3,mix(wave1, wave2,0.5),0.5);
  }
  else{
    wave1  = generateWave(uv1, 0.1, -uv1.x   * iExpand, waveReducer+0.2);
    wave2  = generateWave(uv1, 0.3,  uv1.x   * iExpand, waveReducer+0.2);
    wave3  = generateWave(uv1, 0.2,  1-uv1.x * iExpand, waveReducer+0.2);
    wave4  = generateWave(uv1, 0.25, uv1.x   * iExpand, waveReducer+0.2);

    w = mix(mix(mix(wave1,wave2,0.5), wave3, 0.5), wave4,0.5);
  }

  vec2 o = warpedCords(uv1);

  vec4 cTextureScreen = w;
  float sCount = 900.;
  float nIntensity=0.8;
  float sIntensity=0.9;
  // sample the source
  float x = uv1.x * uv1.y * iGlobalTime *  1000.0;
  x = mod( x, 13.0 ) * mod( x, 123.0 );
  float dx = mod( x, 0.05 );
  vec3 cResult = cTextureScreen.rgb + cTextureScreen.rgb * clamp( 0.1 + dx * 100.0, 0.0, 1.0 );
  // get us a sine and cosine
  vec2 sc = vec2( sin( uv1.y * sCount ), cos( uv1.y * sCount ) );
  // add scanlines
  cResult += cTextureScreen.rgb * vec3( sc.x, sc.y, sc.x ) * sIntensity;
  // interpolate between source and result by intensity
  cResult = cTextureScreen.rgb + clamp(nIntensity, 0.0,1.0 ) * (cResult - cTextureScreen.rgb);

  vec4 f = vec4(cResult, cTextureScreen.a);
  vec4 cutout = textureCutout(f, texture2D(iChannel2, o));

  //vec4 snow = vec4(0.0,0.0,0.0,0.0);
  vec4 snow = generateSnow(uv1, f);

  if(lightOn==1){
    vec3 from=vec3(0.,0.1,-1.2);
    vec3 dir=normalize(vec3(uv,1.));
    float col = light(from, dir);
    col = col;
    gl_FragColor = w * vec4(col);
  }
  else{
    gl_FragColor = f;
  }

}
