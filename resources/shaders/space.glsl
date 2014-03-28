void main()
{
  float v,t=v=.001;
  for (float s=.0; s<2.; s+=.01) {
    vec3 p=s*gl_FragCoord.xyz*t+vec3(.1,.2,fract(s+floor(iGlobalTime*25.)*.01));
    for (int i=0; i<8; i++) p=abs(p)/dot(p,p)-.8;
    v+=dot(p,p)*t;
  }
  gl_FragColor=vec4(v);
}
