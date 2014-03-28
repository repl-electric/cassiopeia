#define globes 250.
#define globesize .13
#define dist 1.2
#define persp .8

mat3 rotmat(vec3 v, float angle)
{
  angle=radians(angle);
  float c = cos(angle);
  float s = sin(angle);

  return mat3(c + (1.0 - c) * v.x * v.x, (1.0 - c) * v.x * v.y - s * v.z, (1.0 - c) * v.x * v.z + s * v.y,
              (1.0 - c) * v.x * v.y + s * v.z, c + (1.0 - c) * v.y * v.y, (1.0 - c) * v.y * v.z - s * v.x,
              (1.0 - c) * v.x * v.z - s * v.y, (1.0 - c) * v.y * v.z + s * v.x, c + (1.0 - c) * v.z * v.z
              );
}


void main(void)
{
  vec2 uv = (gl_FragCoord.xy / iResolution.xy-.5)*2.;
  uv.y*=iResolution.y/iResolution.x;
  float s=.1,maxc=0.;
  vec3 rotv=vec3(0.,0.,1.);
  float h;
  vec3 col=vec3(0.);
  vec3 c=vec3(0.);
  float t=mod(iGlobalTime,15.)*3.;
  float mt=iGlobalTime*.4;
  float et=pow(t*10.,.5);
  mat3 camrot=rotmat(normalize(vec3(0.,1.,0.5)),t*30.);
  for (float i=0.; i<globes; i++) {
    float imt=i+mt;
    vec2 pick=vec2(floor(imt/256.),mod(imt,256.))/256.;
    vec3 p=normalize(texture2D(iChannel0,pick).xyz-.5)*
      (.1+texture2D(iChannel0,pick+vec2(.5,0.)).x*.4);
    p*=length(p)+et*.1;
    vec3 col=normalize(abs(p));
    p*=camrot;
    p.xy*=persp/max(0.001,p.z+dist);
    float siz=globesize*persp/max(0.001,p.z+dist)*clamp(t*.5,0.1,1.);
    c=max(c,col*(pow(max(0.,siz-length(uv+p.xy))/siz,.2))*max(0.15,-p.z));
  }
  c*=3.;
  gl_FragColor = vec4(1.-c,1.);
}
