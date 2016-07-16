Shader "seascapeTestShader" {
	Properties {
		
	}
	SubShader {
		Tags {
			
		}
		Pass {
			
			
			GLSLPROGRAM
			
			#ifdef VERTEX
			
			uniform mat4 _Object2World;
			varying vec4 position_in_world_space;
			void main(void){
				(position_in_world_space = (_Object2World * gl_Vertex));
				(gl_Position = (gl_ModelViewProjectionMatrix * gl_Vertex));
				
			}
			
			#endif
			
			
			#ifdef FRAGMENT
			
			varying vec4 position_in_world_space;
			void main(void){
				vec4 v195;
				if((distance(position_in_world_space,vec4(0.0,0.0,0.0,1.0)) < 5.0)){
					(v195 = vec4(0.0,1.0,0.0,1.0));
					
				}
				else {
					(v195 = vec4(0.3,0.3,0.3,1.0));
					
				}
				(gl_FragColor = v195);
				
			}
			
			#endif
			
			ENDGLSL
		}
		
	}
	
}

