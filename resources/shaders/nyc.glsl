//NYC
uniform float iOvertoneVolume;
uniform float iBeat;
uniform float iBeatCount;
uniform float iMeasureCount;

uniform float iCircleCount;
uniform float iAccelerator;
uniform float iColor;
uniform float iScale;
uniform float iHalfPi;

uniform float iGlobalBeatCount;

const float pi = 3.14159265;
//const float halfpi = 0.000001;
const mat2 m = mat2(0.80,  0.60, -0.60,  0.80);

float rand(vec2 co){
  return fract(sin(dot(co.xy ,vec2(2.9898,78.233))) * 58.5453);
}

float rand2(vec2 co){
  return fract(sin(dot(co.xy ,vec2(12.9898,78.233))) * 43758.5453);
}

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

vec4 populationDensity(vec2 pos)
{
  float person = 0.0;
  float personGirth = 4.0;
  float maximumCapacity = 0.5;
  float walkingSpeed = 0.00000008;
  float populationSize = 0.01;
  float urgencyRate = 0.002;

  //walkingSpeed *=  sin(iBeat+0.5);
  //  incrementSize =+ iOvertoneVolume*0.01;
  float direction = 1.0;

  if(iMeasureCount > 3.0){
    direction = -1.0;
  }
  else{
    direction = 1.0;
  }

  //    if(iBeat == 1){
  for (float i=0.0; i<maximumCapacity; i+=populationSize) {
    float seed = iGlobalTime*walkingSpeed+i;
    //      vec2 point = vec2(rand2(vec2(sin(iGlobalTime*0.0000001), seed)), rand2(vec2(seed, seed)));
    vec2 point = vec2(rand2(vec2(direction*seed, direction*0.5)), rand2(vec2(direction*0.5, direction*seed)));

    if (abs(sqrt(pow(pos.x-point.x,personGirth)+pow(pos.y-point.y-0.1,personGirth))/1.0) < 0.0001) {
      person += (urgencyRate/i) * (iBeat + iMeasureCount * 0.09);
    }
  }
  //    }

  return vec4(vec3(iMeasureCount*0.01+person, iBeatCount*0.01+person, iOvertoneVolume*0.01+person),1.0);
}

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

  arcpos /= pi;
  float arcpos = (pi + atan(rotatedUVs.y, rotatedUVs.x)) / cheese;

  arcpos = smoothstep(0.2, shading - coreident * 0.0001, fract(arcpos) * fract(-arcpos));
  mainval *= fbm4(coreident, iGlobalBeatCount * 0.1) * arcpos;

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
<<<<<<< HEAD
  float amount=0.5;
  float xs = floor(gl_FragCoord.x / size);
  float ys = floor(gl_FragCoord.y / size);
  //vec4 snow = vec4(rand(vec2(xs*iGlobalBeatCount*0.0000008, smoothstep(0.01, 0.02, iGlobalBeatCount))) * amount);
  //  vec4 snow = vec4(rand(vec2(xs/iGlobalBeatCount*202, smoothstep(0.01, 0.02, iGlobalBeatCount))) * amount);
  vec4 snow = vec4(rand(vec2(xs, ys*iGlobalBeatCount*speed))*amount);
=======
  float amount=0.3;
  float xs = floor(gl_FragCoord.x / size);
  float ys = floor(gl_FragCoord.y / size);
  vec4 snow = vec4(rand(vec2(xs,ys*iGlobalBeatCount * speed))*amount);
>>>>>>> db259ffa853077e25b981c40b0b162bd03858898
  return snow;
}

void main(void){
  vec2 uv = gl_FragCoord.xy / iResolution.x;
<<<<<<< HEAD
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
=======
  float snowWeight = 0.4;
  float snowSpeed = 0.00000001; //0.0000000001;

  vec4 c;
  c = circular() + (snowWeight * generateSnow(uv, snowSpeed)) +  populationDensity(uv);
>>>>>>> db259ffa853077e25b981c40b0b162bd03858898
  gl_FragColor = c;
}
