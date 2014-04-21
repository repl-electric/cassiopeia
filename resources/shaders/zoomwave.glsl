uniform float iLColor;
uniform float iRColor;
uniform float iA;
uniform float iRes;
uniform float iSpace;
uniform float iOvertoneVolume;

const int smoothWave = 0;
const float waveReducer = 0.1;

float smoothbump(float center, float width, float x, float orien) {
  float w2 = width/2.0;
  float cp = center+w2;
  float cm = center-w2;
  float c;

  if(orien > 0.0){
    x = orien-x;
  }

  //c = smoothstep(cm, center, x) * (1.0-smoothstep(center, cp, x));
  c = smoothstep(cm, center, x);
  //c = smoothstep(cm, center, 1-x);
  //c = smoothstep(cm, center, x) + smoothstep(cm, center, 1-x);
  //c = smoothstep(cm, center, x) * 1-smoothstep(center, cp, x);

  return c;
}

vec3 hsv2rgb(float time,float mixRate,float v) {
  return mix(vec3(1.0), clamp((abs(fract(time+vec3(2.,3.,1.)/3.)*6.-3.)-1.),0.,1.),mixRate)*v;
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

  vec2  uv     = gl_FragCoord.xy / iResolution.xy;
  vec2  uv2    = gl_FragCoord.xy / iResolution.xy;
  vec2  uv3    = gl_FragCoord.xy / iResolution.xy;

  vec4  wave1  = generateWave(uv2, 0.1,  uv.x);
  vec4  wave2  = generateWave(uv,  0.3, 0.0);
  vec4  wave3  = generateWave(uv3, 0.2,  uv.x);
  vec4  wave4  = generateWave(uv3, 0.25, -uv.x);

  vec4 w = mix(mix(mix(wave1,wave2,0.5), wave3, 0.5), wave4,0.5);

  gl_FragColor = w;
}
