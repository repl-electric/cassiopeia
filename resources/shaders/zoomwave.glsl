float smoothbump(float center, float width, float x) {
  float w2 = width/2.0;
  float cp = center+w2;
  float cm = center-w2;
  float c = smoothstep(cm, center, x) * (1.0-smoothstep(center, cp, x));
  return c;
}

vec3 hsv2rgb(float h,float s,float v) {
  return mix(vec3(1.),clamp((abs(fract(h+vec3(3.,2.,1.)/3.)*6.-3.)-1.),0.,1.),s)*v;
}

uniform float iLColor;
uniform float iRColor;
uniform float iA;
uniform float iRes;
uniform float iSpace;

void main(void)
{
  float space = 0.1;
  float res = 0.75;
  vec2  uv     = gl_FragCoord.xy/iResolution.xy;

  float wave   = texture2D(iChannel0,vec2(uv.x,iRes)).x;
  wave         = smoothbump(0.2,(6.0/iResolution.y), wave+uv.y-0.1); //0.5
  vec3  wc     = wave*hsv2rgb(fract(iGlobalTime/2.0),iLColor,iRColor); //0.1 0.1 0.9 0.9

  float zf     = -0.05;
  vec2  uv2    = (1.0+zf)*uv-(zf/2.0,zf/2.0);
  vec3  pc     = iSpace*texture2D(iChannel1,uv2).rgb;

  gl_FragColor = vec4(vec3(wc+pc),1.0);
}
