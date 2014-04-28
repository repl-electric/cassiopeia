// u2 : developped on my lapop at 2fps at the Revision 2014.
//the live coding session has rised my motivation to start something with shader toy
//Used a lot of template/code from other shaders.
//Old school spiky ball effect.


// Set up a camera looking at the scene.
// origin - camera is positioned relative to, and looking at, this point
// distance - how far camera is from origin
// rotation - about x & y axes, by left-hand screw rule, relative to camera looking along +z
// zoom - the relative length of the lens
void CamPolar( out vec3 pos, out vec3 ray, in vec3 origin, in vec2 rotation, in float distance, in float zoom )
{
  // get rotation coefficients
  vec2 c = vec2(cos(rotation.x),cos(rotation.y));
  vec4 s;
  s.xy = vec2(sin(rotation.x),sin(rotation.y)); // worth testing if this is faster as sin or sqrt(1.0-cos);
  s.zw = -s.xy;

  // ray in view space
  ray.xy = gl_FragCoord.xy - iResolution.xy*.5;
  ray.z = iResolution.y*zoom;
  ray = normalize(ray);
  //localRay = ray;

  // rotate ray
  ray.yz = ray.yz*c.xx + ray.zy*s.zx;
  ray.xz = ray.xz*c.yy + ray.zx*s.yw;

  // position camera
  pos = origin - distance*vec3(c.x*s.y,s.z,c.x*c.y);
}

// return the distance to a sphere
float Psphere( vec3 p, vec3 c, float r )
{
  return length(p-c)-r;
}

float Pcapsule( vec3 p, vec3 a, vec3 b, float rmin, float rmax )
{
  vec3 pa = p - a, ba = b - a;
  float h = clamp( dot(pa,ba)/dot(ba,ba), 0.0, 1.0 );
  vec3 norm = normalize( b - a );
  float d = -dot( norm, a );
  float dist = dot( norm, p ) + d ;

  float rf = dist / length( b-a );

  float r = mix( rmin, rmax, rf*rf*rf );

  return length( pa - ba*h ) - r;
}

// polynomial smooth min (k = 0.1);
float blendPrim( float a, float b  )
{
  float k=0.32 ;
  float h = clamp( 0.5+0.5*(b-a)/k, 0.0, 1.0 );
  return mix( b, a, h ) - k*h*(1.0-h);
  //return min( a, b );
}

// Convert a vector into 2 Auler angle
vec2 ToEuler( vec3 p )
{
  return vec2( atan( p.z, p.x  ),
               atan( sqrt(p.x*p.x+p.z*p.z), p.y)) ;
  //return vec2( atan( p.x, p.z ),
  //     atan( p.y, sqrt(p.x*p.x+p.z*p.z) )) ;
}

vec3 FromEuler( vec2 e )
{
  float c = sin(e.y);
  return vec3( c * cos(e.x),
               cos(e.y),
               c * sin(e.x));
}

float nbSpikes = 3. ;
float PI = 3.141592653 ;
vec3 ClassifyDir( vec3 p, out float id )
{
  vec2 angle = ToEuler( p )  ;

  float div = 2.*PI / nbSpikes;

  angle /= div ;
  angle = floor(angle);
  id = angle.x+angle.y ;
  angle = angle*div;
  angle += div*0.5 ;

  return FromEuler(angle);
  //return normalize(p) ;
  //if( p.y > 0. )
  //return vec3(0.,1.,0.);
  //else
  //return vec3(0.,-1.,0.);
}

float Pcapsule1( vec3 p, vec3 dir, float dist, float thickmin, float thickmax )
{
  return Pcapsule( p, dir*1., dir*(dist-3.), thickmin, thickmax );
}

vec4 QuatFromAxisAngle( vec3 axis, float angle )
{
  float theta = 0.5 * angle;
  float sine = sin(theta);
  return vec4( cos(theta),
               axis.x * sine,
               axis.y * sine,
               axis.z * sine );
}

vec3 TransformVecQuat( vec3 v, vec4 q )
{
  vec3 t = 2. * cross(q.xyz, v);
  return v + q.w * t + cross(q.xyz, t);
}

vec4 PlaneFromNormPos( vec3 n, vec3 p )
{
  return vec4( n.x, n.y, n.z,
               - dot( n, p ) );
}

float Pplane( vec3 p, vec4 n )
{
  // n must be normalized
  return dot(p,n.xyz) + n.w;
}

// Compute the Spiky object
float Spiky( vec3 p )
{
  // transform input position*
  vec4 q = QuatFromAxisAngle( normalize( vec3(cos(iGlobalTime*0.7),1,0) ), iGlobalTime*1.5 - 0.35*length(p) );
  p = TransformVecQuat( p, q );


  // Spiky !
  float id ;
  //float p0 = Pcapsule( p, vec3(0,1,0), vec3(0,5,0), 0.05 );
  vec3 d = ClassifyDir(p,id);
  vec3 infos = vec3( 10., 0.3, 0.02 );
  /*float modc = mod(id,4. ) ;
    if( modc < 1. )
    infos = vec3( 11., 0.2, 0.04 );
    else if( modc < 2. )
    infos = vec3( 9., 0.4, 0.035 );
    else if( modc < 3. )
    infos = vec3( 8., 0.1, 0.055 );*/
  float p0 = Pcapsule1( p, d, infos.x, infos.y, infos.z );
  float p1 = Psphere( p, vec3(0,0,0), 2.5 );

  return blendPrim( p0, p1 );// blend primitives
  //return p1;// blend primitives
}

float PlaneY = -7. ;

// return the distance to the scene
float Scene( vec3 p )
{
  float s  = Spiky( p ) ;

  // Floor and background
  vec4 floorp = PlaneFromNormPos( vec3(0.,1.,0.), vec3(0.,PlaneY,0.) );
  float f = Pplane( p, floorp );

  return min( s, f );
  //return f ;
  //return p1 ;// blend primitives
}


vec3 calcNormal( in vec3 p, out float valid )
{

  vec3 e = vec3(0.001,0.0,0.0);
  vec3 norm = vec3(Scene(p+e.xyy) - Scene(p-e.xyy),
                   Scene(p+e.yxy) - Scene(p-e.yxy),
                   Scene(p+e.yyx) - Scene(p-e.yyx) );
  float l = length( norm );
  //if( l > 0.0015 && l < 1000. )
  //{
  valid = 1. ;
  norm /= l ;
  //}
  //else
  //{
  //valid = 0. ;
  //}
  return norm ;
}

float calcAOSpiky( in vec3 pos, in vec3 nor )
{
  float ao = 1.0;
  float totao = 0.0;
  float sca = 15.0;
  for( int aoi=0; aoi<1 aoi++ )
    {
      float hr = 0.002 + 0.04*float(aoi*aoi);
      vec3 aopos =  nor * hr + pos;
      float dd = Spiky( aopos );
      totao += -(dd-hr)*sca;
      sca *= 0.5;
    }
  return 1.0 - clamp( totao, 0.0, 1.0 );
}

// compute soft shadow from Spiky object
float softShadowSpiky( in vec3 ro, in vec3 rd, in float mint, in float maxt, in float k )
{
  float res = 1.0;
  float dt = 0.02;
  float t = mint;
  for( int i=0; i<1; i++ )
    {
      if( t<maxt )
        {
          float h = Spiky( ro + rd*t );
          res = min( res, k*h/t );
          t += max( 0.05, dt );
        }
    }
  return clamp( res, 0.0, 1.0 );
}

float TraceSpiky( vec3 vpos, vec3 vdir )
{
  float tmax = 80.0;
  float dist = 1.0;
  float t = 0.0;
  for( int i=0; i < 1; i++ )
    {
      if( dist<0.01 || dist>tmax )
        break;

      vec3 p = vpos + t*vdir;
      dist = Spiky( p );

      t += dist ;
    }

  return t ;
}

float TraceFloor( vec3 vpos, vec3 vdir, vec3 viewDir )
{
  float tmax = 80.0;
  float dist = 1.0;
  float t = 0.0;
  for( int i=0; i < 2; i++ )
    {
      if( dist<0.01 || dist>tmax )
        break;

      vec3 p = vpos + t*vdir;

      vec4 floorp = PlaneFromNormPos( vec3(0.,1.,0.), vec3(0.,PlaneY,0.) );
      dist = Pplane( p, floorp );

      //dist = min( dist, dot( p, -viewDir )-60. );

      t += dist ;
    }

  return t ;
}

// main light
vec3 lightDir = vec3( 1, -0.8, 0.5 );
vec3 lightCol = vec3( 0.9, 0.9, 0.9 );

// pseudo sss color
vec3 sssCol = 2.*vec3( 0.12, 0.1, 0.07 );


// fog
vec3 fogCol = vec3(1.,1.,1.);
float fogCutIn = 20. ;
float fogPower = 0.04 ;


float ComputeFog( vec3 pos, vec3 vpos )
{
  // fog
  float lfog = length(pos-vpos) - fogCutIn ; // start the fog a much further from the camera
  return exp( -fogPower*lfog );
}

vec3 ShadeSpiky( vec3 pos, vec3 vpos, vec3 vdir, vec3 viewDir )
{
  float validNorm = 0.;
  vec3 nor = calcNormal( pos, validNorm );
  //vec3 nor = ClassifyDir( pos );

  lightDir = normalize( lightDir );
  float l0i = clamp( -dot( nor, lightDir ), 0., 1. );
  float l0Sh = 1. ;
  if( l0i >0.02 )
    l0Sh = softShadowSpiky( pos, -lightDir, 5., 40., 3. );
  vec3 l0 = l0i * l0Sh * lightCol;


  vec3 lightDir1 = vec3( -1, 1, 0 );
  vec3 lightCol1 = 0.5*vec3( 0.22, 0.2, -0.17 );

  vec3 lightDir2 = normalize( viewDir + vec3(0.5,0.1,0.) );
  vec3 lightCol2 = 0.5*vec3( 0.12, 0.12, 0.2 );

  float sshCoef = 1.-clamp( -dot( nor, viewDir ), 0., 1. );
  sshCoef = pow( sshCoef, 1. );

  // va calculer l'ambient occlusion
  float ao = 1.-calcAOSpiky( pos, nor );

  vec3 l1 = clamp( -dot( nor, lightDir1 ) * lightCol1, 0., 1. );
  vec3 l2 = clamp( -dot( nor, lightDir2 ) * lightCol2, 0., 1. );


  vec3 col = mix( l0+l1, l2, ao ) ;
  col += sshCoef * sssCol * validNorm ;

  // apply fog
  float fi =  ComputeFog( pos, vpos );
  col = mix( fogCol, col, fi );

  return col;
}

vec3 ShadeFloor( vec3 pos, vec3 vpos, vec3 vdir )
{
  float aoSh = calcAOSpiky( pos, vec3(0.,1.,0.) );
  float l0Sh = softShadowSpiky( pos, -lightDir, 4., 10., 3.5 );
  l0Sh *= aoSh;

  vec3 col = mix( vec3(0.85), 0.5*sssCol, 0.4*(1.-l0Sh) ) ;

  // compute reflection :
  vec3 rdir = reflect( vdir, vec3(0,1,0) );
  float ts = TraceSpiky( pos, rdir );
  if( ts < 20. )
    {
      vec3 hpos = pos+rdir*ts ;
      float attY = exp( -0.2*max(hpos.y+8.,0.) );
      col = mix( col, ShadeSpiky( hpos, vpos, rdir, vdir ), attY );
    }


  // apply fog
  float fi =  ComputeFog( pos, vpos );
  col = mix( fogCol, col, fi );

  return col ;
  //return vec3(length( vpos-pos)/120.1);
}



void main(void)
{
  vec2 camRot = vec2(0.4,2.6)+vec2(-2.5,8.5)*(iMouse.yx/iResolution.yx) ;
  camRot.x = max( camRot.x, 0. );
  vec3 vpos, vdir;
  vec3 targetPos = vec3(0.,0.,0.);
  CamPolar( vpos, vdir, targetPos, camRot, 25.0 + 5.*cos(iGlobalTime*0.1), 1.5 );
  vec3 viewDir = normalize( targetPos - vpos );

  vec2 uv = gl_FragCoord.xy / iResolution.xy;

  vec2 p = -1.0 + 2.0*uv;
  p.x *= iResolution.x/iResolution.y;


  float ts = TraceSpiky( vpos, vdir );
  float tf = TraceFloor( vpos, vdir, viewDir );

  vec3 col ;
  if( ts < tf )
    col = ShadeSpiky( vpos+vdir*ts, vpos, vdir, viewDir );
  else
    col = ShadeFloor( vpos+vdir*tf, vpos, vdir );


  gl_FragColor = vec4(col,1.0);
}
