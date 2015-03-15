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

  float time = 1.0;
  float space = 0.0;
  float blurWeight = 1.0;

  uv.y+= 0.0;
  //  uv.x= uv.x*1.0;
  //  uv.y= uv.y*1;

  r = r- time*(textureCutout(vec4(0.0,0.0,0.0,1.0), texture2D(iChannel2, warpedCords(uv))));
  r = r- space*(textureCutout(vec4(0.0,0.0,0.0,1.0),texture2D(iChannel3, warpedCords(uv))));


  gl_FragColor = r; //lineDistort(r, uv);
}
