//NYC
uniform float iOvertoneVolume;
uniform float iBeat;
uniform float iBeatCount;
uniform float iMeasureCount;

uniform float iCircleCount;
uniform float iAccelerator;
uniform float iColor;
uniform float iScale;
uniform float halfpi;

uniform float iGlobalBeatCount;

const float pi = 3.14159265;
//const float halfpi = 0.000001;
const mat2 m = mat2(0.80,  0.60, -0.60,  0.80);

mat2 mm2(in float a){float c = abs( cos(a) ), s = sin(a);return mat2(c,-s,s,c);}
float saturate(float a){ return clamp( a, 0.0, 1.0 );}
// Fractional Brownian Motion code by IQ.
float noise(float x, float y){return sin(1.5*x)*sin(1.5*y);}

float fbm4( float x, float y ){
  vec2 p = vec2( x, y );
  float f = 0.0;
  f += 0.5000*noise(p.x, p.y); p = m*p*2.02;
  f += 0.2500*noise(p.x, p.y); p = m*p*2.03;
  f += 0.1250*noise(p.x, p.y); p = m*p*2.01;
  f += 0.0625*noise(p.x, p.y);
  return f/0.9375;
}

const float linesmooth = 0.0333;

vec4 circular(void){
  vec2 mainuv = (gl_FragCoord.xy / iResolution.xy);

  float aspect = iResolution.x/iResolution.y;
  float finalval = iColor; // 0.1
  float scale = iScale;
  float speed = iAccelerator;
  float shading = 0.20025;
  //relate speed - shading
  float cheese = iHalfPi;

  float circleScale = iCircleCount;
  if (iOvertoneVolume < 0.01) {
    circleScale = 0.0;
    speed = 0.0;
    cheese = 0.000000001;
  }

  vec2 uv = mainuv * scale - scale * 0.5;
  uv.x *= aspect;

  float mainval = 1.0;
  float inverseLength = saturate(length(uv));

  float core = inverseLength * circleScale;
  float coreident = ceil(core);

  vec2 rotatedUVs = uv * mm2(cheese + fbm4(coreident * 0.005 , iGlobalTime * speed * texture2D(iChannel0, vec2(0, 0)).x) * pi * pi);
  rotatedUVs *= mm2(cheese - fbm4(coreident * 2.0 , iGlobalTime * speed * texture2D(iChannel0, vec2(0, 0)).x ) * pi * pi);

  float arcpos = (pi + atan(rotatedUVs.y, rotatedUVs.x)) / halfpi;
  arcpos /= pi;

  arcpos = smoothstep(0.2, shading - coreident * 0.0001, fract(arcpos) * fract(-arcpos));

  mainval *= fbm4(coreident, iGlobalBeatCount * 0.9) * arcpos;

  float coresmooth = fract(core) * fract(-core);
  float corewidth  = fwidth(coresmooth);
  const float edgethreshold = 0.1;
  mainval *= smoothstep(edgethreshold - corewidth, edgethreshold + corewidth, coresmooth);
  finalval += mainval;

  finalval = max(finalval, 0.0) + 0.0025;
  finalval = min(finalval, 1.0);

  return vec4(vec3(pow(finalval, 1.0 / 2.0 )), 1.0);
}

vec4 generateSnow(vec2 p, float speed){
  float size = 2.;
  float amount=0.5;
  float xs = floor(gl_FragCoord.x / size);
  float ys = floor(gl_FragCoord.y / size);
  //vec4 snow = vec4(rand(vec2(xs*iGlobalBeatCount*0.0000008, smoothstep(0.01, 0.02, iGlobalBeatCount))) * amount);
  //  vec4 snow = vec4(rand(vec2(xs/iGlobalBeatCount*202, smoothstep(0.01, 0.02, iGlobalBeatCount))) * amount);
  vec4 snow = vec4(rand(vec2(xs, ys*iGlobalBeatCount*speed))*amount);
  return snow;
}

void main(void){
  vec2 uv = gl_FragCoord.xy / iResolution.x;
  float snowWeight = 0.9;
  float snowSpeed = 0.000000000001; //0.00000001; //0.0000000001;

  float populationWeight = 0.0;
  float circularWeight = 1.0;

  if(iOvertoneVolume > 0.01){
    snowSpeed = 0.0000000001;
    snowWeight = 0.4;
  }
  else{
    snowWeight = 0.0;
  }

  vec4 c;
  c = (circularWeight*circular()) +
      (snowWeight * generateSnow(uv, snowSpeed)) +
      populationWeight*populationDensity(uv);
  gl_FragColor = c;
}
