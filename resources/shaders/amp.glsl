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

vec3 lightdir=-vec3(.1,.0,0.);
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
  w = w + logoAlpha;
  w = negAlpha-w;
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

vec4 generateWave(vec2 uv, float yOffset, float orien){
  float centerOffset=1.4;
  vec4 wa = texture2D(iChannel0, vec2(uv.x, iRes*2.6));
  float wave = waveReducer*wa.x;

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

void main(void)
{
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
  if(ampMode==1){
    vec4  wave1  = generateWave(uv,  0.0, uv.x);
    vec4  wave2  = generateWave(uv,  0.1, uv.x);
    vec4  wave3  = generateWave(uv,  0.05, uv.x);

    w = c * mix(wave3,mix(wave1, wave2,0.5),0.5);
  }
  else{
    vec4  wave1  = generateWave(uv1, 0.1, -uv1.x   * iExpand);
    vec4  wave2  = generateWave(uv1, 0.3, -uv1.x     * iExpand);
    vec4  wave3  = generateWave(uv1, 0.2, 1-uv1.x * iExpand);
    vec4  wave4  = generateWave(uv1, 0.25,uv1.x  * iExpand);

    w = mix(mix(mix(wave1,wave2,0.5), wave3, 0.5), wave4,0.5);
  }

  w = textureCutout(w, texture2D(iChannel2, vec2(uv1.x, -uv1.y)));

  if(lightOn==1){
    vec3 from=vec3(0.,0.1,-1.2);
    vec3 dir=normalize(vec3(uv,1.));
    float col = light(from, dir);
    col = col;
    gl_FragColor = w * vec4(col);
  }
  else{
    gl_FragColor = w;
  }

}
