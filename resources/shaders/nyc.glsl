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
uniform float iInOutSpeed;

uniform float iSnowRatio;
uniform float iDestructure;

uniform float iGlobalBeatCount;

const float pi = 3.14159265;
const mat2 m = mat2(0.80,  0.60, -0.60,  0.80);

const float darkMode = 0.0;

vec3 hsv2rgb(float h, float s, float v) {
  return mix(vec3(1.), clamp((abs(fract(h+vec3(3.,2.,1.)/3.)*6.-3.)-1.),0.,1.),s)*v;
}

float rand(vec2 co){
  return fract(sin(dot(co.xy ,vec2(2.9898,78.233))) * 58.5453);
}

float rand2(vec2 co){
  return fract(sin(dot(co.xy ,vec2(12.9898,78.233))) * 43758.5453);
}

float noise(float x, float y){return sin(1.5*x)*sin(1.5*y);}

mat2 mm2(in float a){
  float c = abs(cos(a));
  float s = sin(a);
  vec2 uv = gl_FragCoord.xy / iResolution.x;
  float FREQ_SCALE = (4096.0/4096.0);
  float AMP_SCALE = 1.0/2.0;
  float fi = FREQ_SCALE*uv.y;
  float fid  = FREQ_SCALE/4096.0/2.0;
  float adjust = AMP_SCALE * 0.5 *   (max(0.0, texture2D(iChannel0, vec2(fi,0.25)).x) +
                                      max(0.0, texture2D(iChannel0, vec2(fi+fid,0.25)).x));

  return mat2(c * adjust, -s * adjust,s * adjust ,c * adjust);
}

float saturate(float a){ return clamp( a, 0.0, 1.0 );}
// Fractional Brownian Motion code by IQ.
// http://en.wikipedia.org/wiki/Fractional_Brownian_motion

float fbm4( float x, float y ){
  vec2 p = vec2( x, y );
  float f = 0.0;

  if(iBeat == 1.0){
    f = .02;
  }

  f +=  0.5000*noise(p.x, p.y);
  p = m*p*2.02 + iMeasureCount * 0.0009;
  f +=  0.2500*noise(p.x, p.y);
  p = m*p*2.03 + iMeasureCount * 0.0009;
  f +=  0.1250*noise(p.x, p.y);
  p = m*p*2.01 + iMeasureCount * 0.0009;
  f +=  0.0625*noise(p.x, p.y);

  return f / (0.9375 + (iMeasureCount * 0.009 * 4));
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

  float mainval = 1.0; //50-60 nice overpaint effect
  float inverseLength;

  if(iDestructure == 1.0){
    inverseLength = saturate(length(uv)) + clamp(iBeat, 0.0, 0.002);
  }
  else{
    inverseLength = saturate(length(uv)) * (uv.x + uv.y) *  iDestructure;
  }

  //inverseLength = saturate(length(uv)) * rand(vec2(texture2D(iChannel0, vec2(0, 0)).x, texture2D(iChannel0, vec2(0, 0)).y));

  float core = inverseLength * circleScale;
  float coreident = ceil(core);

  speed = 1e-4 + clamp(iBeat,0.0,0.0000001);

  float halfpi = cheese;

  shading = clamp(iOvertoneVolume, 0.20025, 0.20025);

  float musiz = texture2D(iChannel0, vec2(0.,0.)).x / 100.0 + iOvertoneVolume/1;
  //
  //vec2 rotatedUVs = uv * mm2( halfpi + fbm4( coreident * 0.005 , iGlobalTime * 0.005) * pi * pi );
  //rotatedUVs *= mm2( halfpi - fbm4( coreident * 2.0 , iGlobalTime * 0.05) * pi * pi )  * musiz;

  //  * texture2D(iChannel0, vec2(0, 0)).x

  vec2 rotatedUVs = uv * mm2( halfpi + fbm4( coreident * 0.005 , iGlobalTime * 0.07 * clamp(texture2D(iChannel0, vec2(0, 0)).x, 0.0, 0.2)) * pi * pi );
   rotatedUVs *= mm2( halfpi - fbm4(coreident * 2.0 , iGlobalTime *  0.1 * clamp(texture2D(iChannel0, vec2(0, 0)).x, 0.0, 1.0) ) * pi * pi );

  //  vec2 rotatedUVs = uv * mm2( halfpi + fbm4( coreident * 0.5, iGlobalTime * 0.07 ) * pi * pi );
  //rotatedUVs          *= mm2( halfpi - fbm4( coreident * 2.0, iGlobalTime * 0.1  ) * pi * pi );
  float arcpos = ( pi + atan( rotatedUVs.y, rotatedUVs.x ) ) / halfpi;
  arcpos /= pi;
  arcpos = smoothstep( 0.2, shading - coreident * 0.0001, fract( arcpos ) * fract( -arcpos ) );


  //  vec2 rotatedUVs = uv * mm2(cheese + fbm4(coreident * 0.005 , iGlobalBeatCount * speed * texture2D(iChannel0, vec2(0, 0)).x) * pi * pi);
  // rotatedUVs *= mm2(cheese - fbm4(coreident * 2.0 , iGlobalBeatCount * speed * texture2D(iChannel0, vec2(0, 0)).x) * pi * pi);

  //float arcpos = (pi + atan(rotatedUVs.y, rotatedUVs.x)) / cheese;
  //arcpos /= pi;

  //arcpos = smoothstep(0.2, shading - coreident * 0.0001, fract(arcpos) * fract(-arcpos));
  mainval *= fbm4(coreident, iGlobalBeatCount * iInOutSpeed) * arcpos;

  float coresmooth = fract(core) * fract(-core);
  float corewidth  = fwidth(coresmooth);
  const float edgethreshold = 0.1;
  mainval *= smoothstep(edgethreshold - corewidth, edgethreshold + corewidth, coresmooth);
  finalval += mainval;

  finalval = max(finalval, 0.0) + 0.0025;
  finalval = min(finalval, 1.0);

  return vec4(vec3(pow(finalval, 1.0/2.0)) - iBeat * 0.1, 1.0);
}

vec4 generateSnow(vec2 p, float speed){
  float size = 2.;
  float xs = floor(gl_FragCoord.x / size);
  float ys = floor(gl_FragCoord.y / size);
  //vec4 snow = vec4(rand(vec2(xs*iGlobalBeatCount*0.0000008, smoothstep(0.01, 0.02, iGlobalBeatCount))));
  //  vec4 snow = vec4(rand(vec2(xs/iGlobalBeatCount*202, smoothstep(0.01, 0.02, iGlobalBeatCount))));
  vec4 snow = vec4(rand(vec2(xs, ys * iGlobalBeatCount * speed)));
  return snow;
}


// original by nimitz https://www.shadertoy.com/view/lsSGzy#, slightly modified

#define ray_brightness 0.3
#define gamma 1.0
#define ray_density 4.5
#define curvature 15.
#define red   4.
#define green 1.0
#define blue  .1

// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
// !!!!!!!!!!!!! UNCOMMENT ONE OF THESE TO CHANGE EFFECTS !!!!!!!!!!!
// MODE IS THE PRIMARY MODE
#define MODE normalize
// #define MODE

#define MODE3 *
//#define MODE3 +

#define MODE2 r +
//#define MODE2

#define DIRECTION +
//#define DIRECTION -

#define SIZE 0.1

#define INVERT /
// #define INVERT *
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

float noise( in vec2 x )
{
  return texture2D(iChannel2, x*.01).x; // INCREASE MULTIPLIER TO INCREASE NOISE
}

// FLARING GENERATOR, A.K.A PURE AWESOME
mat2 m2 = mat2( 0.80,  0.60, -0.60,  0.80 );
float fbm( in vec2 p )
{
  float z=5.;       // EDIT THIS TO MODIFY THE INTENSITY OF RAYS
  float rz = 0.09; // EDIT THIS TO MODIFY THE LENGTH OF RAYS
  p *= 0.25;        // EDIT THIS TO MODIFY THE FREQUENCY OF RAYS
  for (int i= 1; i < 6; i++)
    {
      rz+= abs((noise(p)-0.5)*2.)/z;
      z = z*2.;
      p = p*2.*m2;
    }
  return rz;
}

vec4 flare(void)
{
  float t = DIRECTION iGlobalTime*.3;
  vec2 uv = gl_FragCoord.xy / iResolution.xy-0.5;
  uv.x *= iResolution.x/iResolution.y;
  uv*= curvature* SIZE;

  float r = sqrt(dot(uv,uv)); // DISTANCE FROM CENTER, A.K.A CIRCLE
  float x = dot(MODE(uv), vec2(.5,0.))+t;
  float y = dot(MODE(uv), vec2(.0,.5))+t;

  float val;
  val = fbm(vec2(MODE2 y * ray_density, MODE2 x MODE3 ray_density)); // GENERATES THE FLARING
  val = smoothstep(gamma*.02-.1,ray_brightness+(gamma*0.02-.1)+.001,val);
  val = sqrt(val); // WE DON'T REALLY NEED SQRT HERE, CHANGE TO 15. * val FOR PERFORMANCE

  vec3 col = val INVERT vec3(red,green,blue);
  col = 1.-col; // WE DO NOT NEED TO CLAMP THIS LIKE THE NIMITZ SHADER DOES!
  float rad= 0.2 * texture2D(iChannel0, vec2(0,0.25)).x; // MODIFY THIS TO CHANGE THE RADIUS OF THE SUNS CENTER
  col = mix(col,vec3(1.), rad - 266.667 * r); // REMOVE THIS TO SEE THE FLARING

  return vec4(col,1.0);
}

void main(void){
  vec2 uv = gl_FragCoord.xy / iResolution.x;
  float snowWeight = 0.3;
  float snowSpeed = 0.000000000001; //0.00000001; //0.0000000001;
  vec4 populationResult = vec4(0., 0., 0., 0.);

  float populationWeight = 1.0;
  float circularWeight = 1.0;

  if(iOvertoneVolume > 0.01){
    snowSpeed = 0.0000000001 + (iBeat * 0.00000000000009);
    snowWeight = 0.3;
  }
  else{
    snowWeight = 0.0;
    circularWeight = 0.0;
  }

  if(populationWeight == 1.0){
    snowSpeed = 0.00000001;
    snowWeight = 0.4;
  }

  snowSpeed *= iSnowRatio;

  vec4 c;

  if(populationWeight > 0.0){
    populationResult = populationWeight*populationDensity(uv);
  }

  if(darkMode == 1.0){
    c = 1.0-(circularWeight*circular()) -  (1.0-(snowWeight * generateSnow(uv, snowSpeed))) - populationResult;
  }
  else{
    c = (circularWeight*circular()) - ((snowWeight * generateSnow(uv, snowSpeed))) + populationResult + flare() * 0.01;
  }
  gl_FragColor = c;
}
