uniform float iLColor;
uniform float iRColor;
uniform float iA;
uniform float iRes;
uniform float iSpace;
uniform float iOvertoneVolume;

float smoothbump(float center, float width, float x) {
  float w2 = width/2.0;
  float cp = center+w2;
  float cm = center-w2;
  float c = smoothstep(cm, center, x) * (1.0-smoothstep(center, cp, x));
  return c;
}

vec3 hsv2rgb(float time,float mixRate,float v) {
  return mix(vec3(1.0), clamp((abs(fract(time+vec3(2.,3.,1.)/3.)*6.-3.)-1.),0.,1.),mixRate)*v;
}

vec4 generateWave(vec2 uv, float yOffset){
  float colorChangeRate = 10.0;
  float wave   = texture2D(iChannel0, vec2(uv.x, iRes*2.9)).x;
  wave         = smoothbump(0.2,(6.0/iResolution.y), wave+uv.y-yOffset); //0.5
  vec3  wc     = wave*hsv2rgb(fract(iGlobalTime/colorChangeRate),iLColor,iRColor); //0.1 0.1 0.9 0.9

  float zf     = -0.05;
  vec2  uv2    = (1.0+zf)*uv-(zf/2.0,zf/2.0);
  vec3  pc     = iSpace*texture2D(iChannel1, uv2).rgb;

  return vec4(vec3(wc+pc), 1.0);
}

void main(void)
{
  float space = 0.1;
  float res = 0.75;
  //  vec2  uv2    = (gl_FragCoord.xy*0.1) / iResolution.xy*0.1;

  vec2  uv     = (gl_FragCoord.xy) / iResolution.xy;
  vec2  uv2    = (gl_FragCoord.xy) / iResolution.xy;
  vec2  uv3    = (gl_FragCoord.xy) / iResolution.xy;

  vec4  wave2  = generateWave(uv, 0.0);
  vec4  wave1  = generateWave(uv2, 0.3);
  vec4  wave3  = generateWave(uv3, 0.6);

  // vec4 w = sin(wave1)+tan(wave2)+tan(wave3);
  vec4 w = mix(mix(wave1,wave2,0.5),wave3, 0.5);

  //  gl_FragColor = vec4(vec3(wc+pc),1.0);
  //  gl_FragColor = vec4(,1.0);

  gl_FragColor = w;


}
