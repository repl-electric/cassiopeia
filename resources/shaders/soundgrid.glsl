#define preset9

#ifdef preset1
#define cells vec2(14.,14.)
#define persp 1.7
#define height 1.
#define linewidth .5
#define lineexp 4.
#define brightness .7
#endif


#ifdef preset2
#define OPAQUE_MODE
#define INVERSE
#define cells vec2(16.,16.)
#define persp 1.
#define height 1.5
#define linewidth .1
#define lineexp .5
#define brightness .8
#endif

#ifdef preset3
#define OPAQUE_MODE
#define cells vec2(10.,10.)
#define persp 2.
#define height .75
#define linewidth .05
#define lineexp .5
#define brightness 1.5
#endif

#ifdef preset4
  #define INVERSE
#define cells vec2(6.,25.)
#define persp 1.
#define height 2.
#define linewidth .07
#define lineexp .3
#define brightness .35
#endif

#ifdef preset5
#define cells vec2(17.,18.)
#define persp .6
#define height 2.5
#define linewidth .2
#define lineexp .5
#define brightness .25
#endif

#ifdef preset6
#define INVERSE
#define OPAQUE_MODE
#define cells vec2(15.,15.)
#define persp 2.5
#define height 1.
#define linewidth .05
#define lineexp .5
#define brightness 1.
#endif

#ifdef preset7
//#define INVERSE
//#define OPAQUE_MODE
#define cells vec2(10.,5.)
  #define persp 2.5
#define height 1.
#define linewidth 3.
#define lineexp 6.
#define brightness .4
#endif



#define hcells (cells*.5)


vec3 segment(vec2 p, vec3 from, vec3 to, float width, float dist) {
  width=1./width;
  vec2 seg=from.xy-to.xy;
  float halfdist=distance(from.xy,to.xy)*.5;
  float ang=atan(seg.y,seg.x);
  float sine=sin(ang);
  float cose=cos(ang);
  p-=from.xy;
  p*=mat2(cose,sine,-sine,cose);
  float dx=abs(p.x+halfdist)-halfdist;
  float dy=abs(p.y);
  float h=1.-abs(p.x+halfdist*2.)/halfdist/2.;
  float pz=-from.z-(to.z-from.z)*h;
  float l=1.-clamp(max(dx,dy)*width/(pz+dist)*dist*dist,0.,.1)/.1;
  l=pow(abs(l),lineexp)*(1.-pow(clamp(abs(dist-pz)*.45,0.,1.),.5))*4.;
  return normalize(.25+abs(mix(from,to,h)))*l;
}

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

float getz(vec2 xy) {
  xy=xy*10.+hcells;
  //float pos=length(pow(abs(xy/cells),vec2(3.)))*8.;
  float pos=(xy.y*cells.x+xy.x)/(cells.x*cells.y);
  float s=texture2D(iChannel0,vec2(.5+pos*.5,.1)).x;
  return .25-pow(s,1.5)*height;
}

void main(void)
{
  vec2 uv = (gl_FragCoord.xy / iResolution.xy-.5)*2.;
  uv.y*=iResolution.y/iResolution.x;
  mat3 camrot=rotmat(normalize(vec3(0.,0.,1.)),iGlobalTime*25.)*rotmat(normalize(vec3(1.,0.*sin(iGlobalTime*.5),0.)),60.+30.*sin(iGlobalTime*.5));
  float s=.1,maxc=0.;
  vec3 p1,p2,p3;
  vec3 rotv=vec3(0.,0.,1.);
  float h;
  vec3 col=vec3(0.);
  float dist=1.2+pow(abs(sin(iGlobalTime*.3)),5.)*.5;
  vec3 c=vec3(0.);
  for (float y=0.; y<cells.y; y++) {
    for (float x=0.; x<cells.x; x++) {
      p1=vec3(x-hcells.x,y-hcells.y,0.)*.1; p1.z=getz(p1.xy);
      p2=vec3(p1.x+.1,p1.y   ,0.); p2.z=getz(p2.xy);
      p3=vec3(p1.x   ,p1.y+.1,0.); p3.z=getz(p3.xy);
      p1*=camrot; p2*=camrot; p3*=camrot;
      p1.xy*=persp/max(0.1,p1.z+dist);
      p2.xy*=persp/max(0.1,p2.z+dist);
      p3.xy*=persp/max(0.1,p3.z+dist);
      if (max(p1.x,p2.x)>uv.x-linewidth/4. && min(p1.x,p2.x)<uv.x+linewidth/4. && x<cells.x-1.) {
        if (max(p1.y,p2.y)>uv.y-linewidth/4. && min(p1.y,p2.y)<uv.y+linewidth/4.) {
          #ifdef OPAQUE_MODE
          c=max(c,segment(uv,p1,p2,linewidth,dist)*1.5);
          #else
          c+=segment(uv,p1,p2,linewidth,dist);
          #endif
        }
      }
      if (max(p1.x,p3.x)>uv.x-linewidth/4. && min(p1.x,p3.x)<uv.x+linewidth/4. && y<cells.y-1.) {
        if (max(p1.y,p3.y)>uv.y-linewidth/4. && min(p1.y,p3.y)<uv.y+linewidth/4.) {
          #ifdef OPAQUE_MODE
          c=max(c,segment(uv,p1,p3,linewidth,dist)*1.5);
          #else
          c+=segment(uv,p1,p3,linewidth,dist);
          #endif
        }
      }
    }
  }
  c*=brightness;
#ifdef INVERSE
  gl_FragColor = vec4(1.-c,1.);
#else
  gl_FragColor = vec4(c,1.);
#endif
}
