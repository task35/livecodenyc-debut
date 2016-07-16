Shader "demoShaderA" {
	Properties {
		
	}
	SubShader {
		Tags {
			"Queue" = "Transparent"
			
		}
		Pass {
			ZWrite Off
			
			Blend SrcAlpha OneMinusSrcAlpha
			
			GLSLPROGRAM
			
			#ifdef VERTEX
			
			uniform mat4 _Object2World;
			uniform vec4 _Time;
			varying vec4 position_in_world_space;
			varying vec4 vtime;
			void main(void){
				(position_in_world_space = (_Object2World * gl_Vertex));
				(vtime = _Time);
				(gl_Position = (gl_ModelViewProjectionMatrix * gl_Vertex));
				
			}
			
			#endif
			
			
			#ifdef FRAGMENT
			
			varying vec4 vtime;
			varying vec4 position_in_world_space;
			void main(void){
				vec2 v1820;
				float v1824;
				vec2 v1822;
				vec2 v1823;
				float v1791;
				vec2 v1819;
				vec2 v1821;
				float v1825;
				float v1779;
				(v1779 = distance(position_in_world_space,vec4(0.0,0.0,0.0,1.0)));
				(v1791 = (((2.0 * 3.141593) * v1779) * 0.008333334));
				(v1819 = (0.9 * vec2(position_in_world_space[int(0.0)],position_in_world_space[int(1.0)])));
				(v1820 = floor(v1819));
				(v1821 = fract(v1819));
				(v1822 = vec2((2.0 * v1821[int(0.0)]),(2.0 * v1821[int(1.0)])));
				(v1823 = ((v1821 * v1821) * vec2((3.0 - v1822[int(0.0)]),(3.0 - v1822[int(1.0)]))));
				(v1824 = v1823[int(0.0)]);
				(v1825 = v1823[int(1.0)]);
				(gl_FragColor = vec4(0.0,0.0,0.0,((vtime[int(1.0)] / 100.0) * ((mod(v1779,13.0) / 13.0) - smoothstep((1.0 - (1.0 / v1791)),1.0,(abs((mod((((((((((fract((sin(dot((vec2(0.0) + vec2(v1820)),vec2(127.1,311.7))) * 43758.545312)) * (1.0 - v1824)) + (fract((sin(dot((vec2(1.0,0.0) + vec2(v1820)),vec2(127.1,311.7))) * 43758.545312)) * v1824)) * (1.0 - v1825)) + (((fract((sin(dot((vec2(0.0,1.0) + vec2(v1820)),vec2(127.1,311.7))) * 43758.545312)) * (1.0 - v1824)) + (fract((sin(dot((vec2(1.0) + vec2(v1820)),vec2(127.1,311.7))) * 43758.545312)) * v1824)) * v1825)) * 2.0) - 1.0) / v1791) * 2.0) + (mod(((atan(position_in_world_space[int(0.0)],position_in_world_space[int(1.0)]) + 3.141593) / (3.141593 * 2.0)),0.008333334) / 0.008333334)),1.0) - 0.5)) * 2.0))))));
				
			}
			
			#endif
			
			ENDGLSL
		}
		
	}
	
}

