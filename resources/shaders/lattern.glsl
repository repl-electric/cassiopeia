// Created by inigo quilez - iq/2013
// License Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License.

// Another example of using a 2D grid to accelerate raytracing (of procedural content in
// this case). The lighting is shadowless this time, that's why it's fast. The ambient
// occlusion is half-procedural, half-analytical.

// For information on analytical ambient occlusion from spheres, see here:
// http://www.iquilezles.org/www/articles/sphereao/sphereao.htm

#define VIS_SAMPLES 6

float hash1( float n ) { return fract(43758.5453123*sin(n)); }
float hash1( vec2  n ) { return fract(43758.5453123*sin(dot(n,vec2(1.0,113.0)))); }
vec2  hash2( float n ) { return fract(43758.5453123*sin(vec2(n,n+1.0))); }
vec3  hash3( vec2  n ) { return fract(43758.5453123*sin(dot(n,vec2(1.0,113.0))+vec3(0.0,1.0,2.0))); }
vec4  hash4( vec2  n ) { return fract(43758.5453123*sin(dot(n,vec2(1.0,113.0))+vec4(0.0,1.0,2.0,3.0))); }

//------------------------------------------------------------

vec4 makeSphere( vec2 pos )
{
  vec3  rr = hash3(pos);
  float ha = 0.2 + 1.3*rr.z;
  vec2  oo = 0.5 + 0.3*(-1.0 + 2.0*rr.xy);
  vec3  ce = vec3( pos.x+oo.x, ha, pos.y+oo.y );
  float ra = (0.5+0.5*rr.z)*min( min(oo.x,1.0-oo.x), min(oo.y,1.0-oo.y) );
  ra *= 0.85+0.15*sin( 1.5*iGlobalTime + hash1(pos)*130.0 );

  ce.y += 0.3*smoothstep( 0.995, 0.996, sin(0.015*iGlobalTime+100.0*hash1(hash1(pos))) );

  return vec4( ce, ra );
}

vec3 palette( float id )
{
  return 0.5 + 0.5*sin( 2.0*id + 1.3 + vec3(0.0,1.0,2.0) );
}

vec3 makeColor( in vec2 p )
{
  float id  = hash1( p );
  return palette( id );
}

vec3 makeEmission( in vec2 p )
{
  float id  = hash1( p );
  vec3 mate =palette( id );
  return mate * smoothstep( 0.995, 0.998, sin(0.015*iGlobalTime+100.0*hash1(id)) );
}

//------------------------------------------------------------


vec4 castRay( in vec3 ro, in vec3 rd )
{
  vec2 pos = floor(ro.xz);
  vec2 ri = 1.0/rd.xz;
  vec2 rs = sign(rd.xz);
  vec2 ris = ri*rs;
  vec2 dis = (pos-ro.xz+ 0.5 + rs*0.5) * ri;

  vec4 res = vec4( -1.0, 0.0, 0.0, 0.0 );

  // traverse regular grid (in 2D)
  for( int i=0; i<24; i++ )
    {
      if( res.x>0.0 ) continue;

      // intersect sphere
      vec4  sph = makeSphere( pos );

      vec3  rc = ro - sph.xyz;
      float b = dot( rd, rc );
      float c = dot( rc, rc ) - sph.w*sph.w;
      float h = b*b - c;
      if( h>0.0 )
        {
          float s = -b - sqrt(h);
          res = vec4( s, 0.0, pos );
        }
      else
        {
          float a = dot( rd.xz, rd.xz );
          b = dot( rc.xz, rd.xz );
          c = dot( rc.xz, rc.xz ) - min(0.25*sph.w*sph.w,0.005);
          h = b*b - a*c;
          if( h>=0.0 )
            {
              // cylinder
              float s = (-b - sqrt( h ))/a;
              if( s>0.0 && (ro.y+s*rd.y)<sph.y )
                {
                  res = vec4( s, 1.0, pos );
                }
            }
        }

      // step to next cell
      vec2 mm = step( dis.xy, dis.yx );
      dis += mm*ris;
      pos += mm*rs;
    }

  return res;
}



vec3 calcNormal( in vec3 pos, in float ic )
{
  if( ic>1.5 ) return vec3(0.0,1.0,0.0);
  return normalize(pos*vec3(1.0,1.0-ic,1.0));
}

float occSphere( in vec4 sph, in vec3 pos, in vec3 nor )
{
  vec3 di = sph.xyz - pos;
  float l = length(di);
  return 1.0 - max(0.0,dot(nor,di/l))*sph.w*sph.w/(l*l);
}

float emmSphere( in vec4 sph, in vec3 pos, in vec3 nor )
{
  vec3 di = sph.xyz - pos;
  float l = length(di);
  float at = 1.0-smoothstep(0.5,2.0,l);
  return at * pow(max(0.0,0.5+0.5*dot(nor,di/l)),2.0)*sph.w*sph.w/(l*l);
}

vec4 texcube( sampler2D sam, in vec3 p, in vec3 n )
{
  vec4 x = texture2D( sam, p.yz );
  vec4 y = texture2D( sam, p.zx );
  vec4 z = texture2D( sam, p.xy );
  return x*abs(n.x) + y*abs(n.y) + z*abs(n.z);
}

vec3 cameraPath( float t )
{
  // procedural path
  vec2 p  = 100.0*sin( 0.02*t*vec2(1.2,1.0) + vec2(0.1,0.9) );
  p +=  50.0*sin( 0.04*t*vec2(1.1,1.3) + vec2(1.0,4.5) );
  float y = 3.5 + 1.5*sin(0.1*t);

  return vec3( p.x, y, p.y );
}
void main( void )
{
  // inputs
  vec2 q = gl_FragCoord.xy / iResolution.xy;

  vec2 mo = iMouse.xy / iResolution.xy;
  if( iMouse.w<=0.00001 ) mo=vec2(0.0);


  // montecarlo
  vec3 tot = vec3(0.0);
    #if VIS_SAMPLES<2
  int a = 0;
  {
    vec2 p = -1.0 + 2.0*(gl_FragCoord.xy) / iResolution.xy;
    p.x *= iResolution.x/ iResolution.y;
    float time = 0.3*iGlobalTime + 50.0*mo.x;
    #else
    for( int a=0; a<VIS_SAMPLES; a++ )
      {
        vec4 rr = texture2D( iChannel1, (gl_FragCoord.xy+floor(256.0*hash2(float(a))))/iChannelResolution[1].xy );
        vec2 p = -1.0 + 2.0*(gl_FragCoord.xy+rr.xz) / iResolution.xy;
        p.x *= iResolution.x/ iResolution.y;
        float time = 0.3*(iGlobalTime + 1.0*(0.5/24.0)*rr.w) + 50.0*mo.x;
#endif

        // camera
        vec3  ro = cameraPath( time );
        vec3  ta = cameraPath( time*2.0+15.0 );
        ta = ro + normalize(ta-ro);
        ta.y = ro.y - 0.4;

        float cr = -0.2*cos(0.1*time);

        // build ray
        vec3 ww = normalize( ta - ro);
        vec3 uu = normalize(cross( vec3(sin(cr),cos(cr),0.0), ww ));
        vec3 vv = normalize(cross(ww,uu));
        float r2 = p.x*p.x*0.32 + p.y*p.y;
        p *= (7.0-sqrt(37.5-11.5*r2))/(r2+1.0);
        vec3 rd = normalize( p.x*uu + p.y*vv + 3.0*ww );

        // dof
        #if VIS_SAMPLES>2
        float fft = (ro.y*2.0+0.0)/dot(rd,ww);
        vec3 fp = ro + rd * fft;
        vec2 bo = sqrt(rr.y)*vec2(cos(6.2831*rr.w),sin(6.2831*rr.w));
        ro += (uu*bo.x + vv*bo.y)*0.005*fft;
        rd = normalize( fp - ro );
        #endif


        // background color
        vec3 bgcol = vec3(0.0);

        vec3 col = bgcol;


        // raytrace top bounding plane
        float tp = (2.3-ro.y)/rd.y;
        if( tp>0.0 ) ro = ro + rd*tp;

        // trace linterns
        vec4 res  = castRay(  ro, rd );

        float tp2 = (0.0-ro.y)/rd.y;
        vec4 res2 = vec4(tp2,2.0,floor(ro.xz+tp2*rd.xz));
        if( res.x<0.0 ) res = res2; else if( tp2<res.x ) res = res2;


        float t = res.x;
        vec2 vos = res.zw;
        if( t>0.0 )
          {
            vec3  pos = ro + rd*t;
            float id  = hash1( vos );

            vec4 sph = makeSphere( vos );

            vec3 rpos = pos-sph.xyz;

            vec3  nor = calcNormal( rpos, res.y );

            // material
            vec3 mate = makeColor( vos );
            if( res.y>1.5 ) mate=vec3(0.15);
            mate *= 0.5 + 1.5*pow(texcube( iChannel0, pos, nor ).x, 1.5 );

            // procedural occlusion
            float occ = (0.5+0.5*nor.y);
            if( res.y<1.5)
              {
                occ*= 0.3+0.7*clamp( pos.y/.24, 0.0, 1.0 );
                if( res.y>0.5 )occ *= 0.6+0.5*clamp( -(pos.y-(sph.y-sph.w))*7.0, 0.0, 1.0 );


              }
            else
              {
                occ *= 0.5 + 0.5*smoothstep(0.0,0.3, length(rpos.xz) );
                occ *= 0.5;
              }
            // analytic occlusion
            float nocc = 1.0;
            nocc *= occSphere( makeSphere(vos+vec2( 1.0, 0.0)), pos, nor );
            nocc *= occSphere( makeSphere(vos+vec2(-1.0, 0.0)), pos, nor );
            nocc *= occSphere( makeSphere(vos+vec2( 0.0, 1.0)), pos, nor );
            nocc *= occSphere( makeSphere(vos+vec2( 0.0,-1.0)), pos, nor );
            if( res.y>1.5 ) nocc *= occSphere( makeSphere(vos+vec2( 0.0,0.0)), pos, nor );
            occ *= nocc*nocc;

            // ambient and emmision
            vec3 amb = vec3(0.015);
            vec3 emm = 1.5*makeEmission(vos)*step(res.y,1.5);

            // direct lichting
            vec3 dir = vec3(0.0);
            float ia = 20.0;
            dir += ia*emmSphere( makeSphere(vos+vec2( 1.0, 0.0)), pos, nor )*makeEmission(vos+vec2( 1.0, 0.0));
            dir += ia*emmSphere( makeSphere(vos+vec2(-1.0, 0.0)), pos, nor )*makeEmission(vos+vec2(-1.0, 0.0));
            dir += ia*emmSphere( makeSphere(vos+vec2( 0.0, 1.0)), pos, nor )*makeEmission(vos+vec2( 0.0, 1.0));
            dir += ia*emmSphere( makeSphere(vos+vec2( 0.0,-1.0)), pos, nor )*makeEmission(vos+vec2( 0.0,-1.0));
            dir += ia*emmSphere( makeSphere(vos+vec2( 1.0, 1.0)), pos, nor )*makeEmission(vos+vec2( 1.0, 1.0));
            dir += ia*emmSphere( makeSphere(vos+vec2(-1.0, 1.0)), pos, nor )*makeEmission(vos+vec2(-1.0, 1.0));
            dir += ia*emmSphere( makeSphere(vos+vec2( 1.0,-1.0)), pos, nor )*makeEmission(vos+vec2( 1.0,-1.0));
            dir += ia*emmSphere( makeSphere(vos+vec2(-1.0,-1.0)), pos, nor )*makeEmission(vos+vec2(-1.0,-1.0));
            dir += ia*emmSphere( makeSphere(vos+vec2( 0.0, 0.0)), pos, nor )*makeEmission(vos+vec2( 0.0, 0.0));

            // lighitng
            vec3 lin = vec3(0.0);
            lin += emm;
            lin += amb*occ;
            lin += dir*occ;
            lin += (amb*0.2+emm+dir) * 40.0 * pow( clamp( 1.0+dot(rd,nor), 0.0, 1.0), 2.0 )*occ*mate;

            if( res.y<1.5 ) lin *= clamp(pos.y,0.0,1.0);

            col = mate * lin;

            // fog
            col *= exp(-0.005*t*t);
          }

        col = clamp(col,0.0,1.0);
        tot += col;
      }
    tot /= float(VIS_SAMPLES);

    tot = pow( clamp(tot,0.0,1.0), vec3(0.44) );

    gl_FragColor = vec4( tot, 1.0 );
  }
