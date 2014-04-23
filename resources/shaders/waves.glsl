uniform float iLColor;
uniform float iRColor;
uniform float iA;
uniform float iRes;
uniform float iSpace;
uniform float iExpand;
uniform float iYinYan;
uniform float iOvertoneVolume;
uniform float iBeat;

const float scale=50.5;
const float detail=50.5;
const float width=0.001;

const int smoothWave=0;
const float waveReducer=0.1;
const int lightOn=1;

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
  float w2 = width/2.0;
  float cp = center+w2;
  float cm = center-w2;
  float c;

  if(orien > 0.0){
    x = orien-x;
  }

  if(iYinYan > 0.0){
    c = iYinYan*smoothstep(cm, center, x);
  }
  else{
    c = smoothstep(cm, center, x) * 1-smoothstep(center, cp, x);
  }
  return c;
}

vec3 hsv2rgb(float time,float mixRate,float v) {
  vec3 c = clamp((abs(fract(time+vec3(2.,3.,1.)/3.)*6.-3.)-1.),0.,1.);
  //  c = c*1.0+iBeat;
  c = c * clamp(iBeat,0.1, 0.3)+0.7;
  return mix(vec3(1.0), c, mixRate)*v;
}

vec4 generateWave(vec2 uv, float yOffset, float orien){
  float colorChangeRate = 10.0;
  vec4 wa = texture2D(iChannel0, vec2(uv.x, iRes*2.9));
  float wave = waveReducer*wa.x;

  if(smoothWave==0){
    wave = smoothbump(0.2,(6.0/iResolution.y), wave+uv.y-yOffset, orien); //0.5
    //wave = (-1.0 * wave)+0.5;
  }
  vec3  wc     = wave * hsv2rgb(fract(iGlobalTime/colorChangeRate),iLColor,iRColor);
//0.1 0.1 0.9 0.9

  float zf     = -0.05;
  vec2  uv2    = (1.0+zf)*uv-(zf/2.0,zf/2.0);
  vec3  pc     = iSpace*texture2D(iChannel1, uv2).rgb;

  return vec4(vec3(wc+pc), 1.0);
}

void main(void)
{
  float space = 0.1;
  float res = 0.75;

  vec2 uv;
  vec2 uv2;
  vec2 uv3;

  uv     = (gl_FragCoord.xy / iResolution.xy);
  uv2    = (gl_FragCoord.xy / iResolution.xy);
  uv3    = (gl_FragCoord.xy / iResolution.xy);

  //uv.x = uv.x * iBeat;
  //uv3.x = uv3.x * iBeat;

  vec4  wave1  = generateWave(uv2, 0.1, uv.x    * iExpand);
  vec4  wave2  = generateWave(uv,  0.3, 0.0     * iExpand);
  vec4  wave3  = generateWave(uv3, 0.2, 1-uv.x  * iExpand);
  vec4  wave4  = generateWave(uv3, 0.25,-uv.x    * iExpand);

  vec4 w = mix(mix(mix(wave1,wave2,0.5), wave3, 0.5), wave4,0.5);

  if(lightOn==1){
    vec3 from=vec3(0.,0.1,-1.2);
    vec3 dir=normalize(vec3(uv,1.));
    float col = light(from, dir);
    col = col;
    gl_FragColor = w*vec4(col);
  }
  else{
    gl_FragColor = w;
  }

}
