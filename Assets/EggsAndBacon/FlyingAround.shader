// Made with Amplify Shader Editor v1.9.3.3
// Available at the Unity Asset Store - http://u3d.as/y3X 
Shader "FlyingAround"
{
	Properties
	{
		[HideInInspector] _EmissionColor("Emission Color", Color) = (1,1,1,1)
		[HideInInspector] _AlphaCutoff("Alpha Cutoff ", Range(0, 1)) = 0.5
		_TextureSample0("Texture Sample 0", 2D) = "white" {}
		_RadiusBottom("RadiusBottom", Range( 0 , 4)) = 0
		_OffsetFromHose("OffsetFromHose", Range( 0 , 5)) = 0
		_OffsetFromHoseNoise("OffsetFromHoseNoise", Range( 0 , 3)) = 1
		_RadiusTop("RadiusTop", Range( 0 , 4)) = 2.809793
		_BendStrength("BendStrength", Range( -5 , 5)) = 0
		_RotationSpeedNoise("RotationSpeedNoise", Range( 0 , 1)) = 0
		_RotationSpeed("RotationSpeed", Float) = 0
		_TwistTexture("TwistTexture", 2D) = "white" {}
		_TwistStrength("TwistStrength", Range( 0 , 5)) = 0
		_TwistScrollX("TwistScrollX", Range( 0 , 5)) = 0
		_TwistScale("TwistScale", Range( 0 , 1)) = 1
		_TwistTop("TwistTop", Range( 0.8 , 2)) = 1
		_WaveStrengthY("WaveStrengthY", Range( 0 , 5)) = 0
		_WaveTexScrollSpeed("WaveTexScrollSpeed", Range( 0 , 2)) = 1
		_RadiusLerpExpo("RadiusLerpExpo", Range( 0 , 5)) = 0.1
		_WaveTexScale("WaveTexScale", Range( 0 , 2)) = 1
		_HueShift("HueShift", Range( -1 , 1)) = 0
		_ValueMulti("ValueMulti", Range( 0 , 10)) = 0
		_SaturationMulti("SaturationMulti", Range( 0 , 10)) = 1
		_TexVsColor("TexVsColor", Range( 0 , 1)) = 0
		[HDR]_Color0("Color 0", Color) = (0,0,0,0)
		[HDR]_Color1("Color 1", Color) = (0,0,0,0)
		_LocalRotationAxis("LocalRotationAxis", Vector) = (1,1,1,0)
		_LocalRotationSpeed("LocalRotationSpeed", Float) = 0
		_TwistOffsetLag("TwistOffsetLag", Range( -0.1 , 0.1)) = 0
		_WrapAroundTwistedHose("WrapAroundTwistedHose", Range( 0 , 1)) = 0
		_ColorGradient("ColorGradient", Range( 0 , 10)) = 0
		_ColorGradientAddToVertexY("ColorGradientAddToVertexY", Range( 0 , 1)) = 0
		_RotateSlowerOnTop("RotateSlowerOnTop", Range( 0 , 1)) = 0
		[HideInInspector] _texcoord( "", 2D ) = "white" {}


		//_TessPhongStrength( "Tess Phong Strength", Range( 0, 1 ) ) = 0.5
		//_TessValue( "Tess Max Tessellation", Range( 1, 32 ) ) = 16
		//_TessMin( "Tess Min Distance", Float ) = 10
		//_TessMax( "Tess Max Distance", Float ) = 25
		//_TessEdgeLength ( "Tess Edge length", Range( 2, 50 ) ) = 16
		//_TessMaxDisp( "Tess Max Displacement", Float ) = 25

		[HideInInspector] _QueueOffset("_QueueOffset", Float) = 0
        [HideInInspector] _QueueControl("_QueueControl", Float) = -1

        [HideInInspector][NoScaleOffset] unity_Lightmaps("unity_Lightmaps", 2DArray) = "" {}
        [HideInInspector][NoScaleOffset] unity_LightmapsInd("unity_LightmapsInd", 2DArray) = "" {}
        [HideInInspector][NoScaleOffset] unity_ShadowMasks("unity_ShadowMasks", 2DArray) = "" {}

		[HideInInspector][ToggleOff] _ReceiveShadows("Receive Shadows", Float) = 1.0
	}

	SubShader
	{
		LOD 0

		

		Tags { "RenderPipeline"="UniversalPipeline" "RenderType"="Opaque" "Queue"="Geometry" "UniversalMaterialType"="Unlit" }

		Cull Off
		AlphaToMask Off

		

		HLSLINCLUDE
		#pragma target 4.5
		#pragma prefer_hlslcc gles
		// ensure rendering platforms toggle list is visible

		#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Common.hlsl"
		#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Filtering.hlsl"

		#ifndef ASE_TESS_FUNCS
		#define ASE_TESS_FUNCS
		float4 FixedTess( float tessValue )
		{
			return tessValue;
		}

		float CalcDistanceTessFactor (float4 vertex, float minDist, float maxDist, float tess, float4x4 o2w, float3 cameraPos )
		{
			float3 wpos = mul(o2w,vertex).xyz;
			float dist = distance (wpos, cameraPos);
			float f = clamp(1.0 - (dist - minDist) / (maxDist - minDist), 0.01, 1.0) * tess;
			return f;
		}

		float4 CalcTriEdgeTessFactors (float3 triVertexFactors)
		{
			float4 tess;
			tess.x = 0.5 * (triVertexFactors.y + triVertexFactors.z);
			tess.y = 0.5 * (triVertexFactors.x + triVertexFactors.z);
			tess.z = 0.5 * (triVertexFactors.x + triVertexFactors.y);
			tess.w = (triVertexFactors.x + triVertexFactors.y + triVertexFactors.z) / 3.0f;
			return tess;
		}

		float CalcEdgeTessFactor (float3 wpos0, float3 wpos1, float edgeLen, float3 cameraPos, float4 scParams )
		{
			float dist = distance (0.5 * (wpos0+wpos1), cameraPos);
			float len = distance(wpos0, wpos1);
			float f = max(len * scParams.y / (edgeLen * dist), 1.0);
			return f;
		}

		float DistanceFromPlane (float3 pos, float4 plane)
		{
			float d = dot (float4(pos,1.0f), plane);
			return d;
		}

		bool WorldViewFrustumCull (float3 wpos0, float3 wpos1, float3 wpos2, float cullEps, float4 planes[6] )
		{
			float4 planeTest;
			planeTest.x = (( DistanceFromPlane(wpos0, planes[0]) > -cullEps) ? 1.0f : 0.0f ) +
							(( DistanceFromPlane(wpos1, planes[0]) > -cullEps) ? 1.0f : 0.0f ) +
							(( DistanceFromPlane(wpos2, planes[0]) > -cullEps) ? 1.0f : 0.0f );
			planeTest.y = (( DistanceFromPlane(wpos0, planes[1]) > -cullEps) ? 1.0f : 0.0f ) +
							(( DistanceFromPlane(wpos1, planes[1]) > -cullEps) ? 1.0f : 0.0f ) +
							(( DistanceFromPlane(wpos2, planes[1]) > -cullEps) ? 1.0f : 0.0f );
			planeTest.z = (( DistanceFromPlane(wpos0, planes[2]) > -cullEps) ? 1.0f : 0.0f ) +
							(( DistanceFromPlane(wpos1, planes[2]) > -cullEps) ? 1.0f : 0.0f ) +
							(( DistanceFromPlane(wpos2, planes[2]) > -cullEps) ? 1.0f : 0.0f );
			planeTest.w = (( DistanceFromPlane(wpos0, planes[3]) > -cullEps) ? 1.0f : 0.0f ) +
							(( DistanceFromPlane(wpos1, planes[3]) > -cullEps) ? 1.0f : 0.0f ) +
							(( DistanceFromPlane(wpos2, planes[3]) > -cullEps) ? 1.0f : 0.0f );
			return !all (planeTest);
		}

		float4 DistanceBasedTess( float4 v0, float4 v1, float4 v2, float tess, float minDist, float maxDist, float4x4 o2w, float3 cameraPos )
		{
			float3 f;
			f.x = CalcDistanceTessFactor (v0,minDist,maxDist,tess,o2w,cameraPos);
			f.y = CalcDistanceTessFactor (v1,minDist,maxDist,tess,o2w,cameraPos);
			f.z = CalcDistanceTessFactor (v2,minDist,maxDist,tess,o2w,cameraPos);

			return CalcTriEdgeTessFactors (f);
		}

		float4 EdgeLengthBasedTess( float4 v0, float4 v1, float4 v2, float edgeLength, float4x4 o2w, float3 cameraPos, float4 scParams )
		{
			float3 pos0 = mul(o2w,v0).xyz;
			float3 pos1 = mul(o2w,v1).xyz;
			float3 pos2 = mul(o2w,v2).xyz;
			float4 tess;
			tess.x = CalcEdgeTessFactor (pos1, pos2, edgeLength, cameraPos, scParams);
			tess.y = CalcEdgeTessFactor (pos2, pos0, edgeLength, cameraPos, scParams);
			tess.z = CalcEdgeTessFactor (pos0, pos1, edgeLength, cameraPos, scParams);
			tess.w = (tess.x + tess.y + tess.z) / 3.0f;
			return tess;
		}

		float4 EdgeLengthBasedTessCull( float4 v0, float4 v1, float4 v2, float edgeLength, float maxDisplacement, float4x4 o2w, float3 cameraPos, float4 scParams, float4 planes[6] )
		{
			float3 pos0 = mul(o2w,v0).xyz;
			float3 pos1 = mul(o2w,v1).xyz;
			float3 pos2 = mul(o2w,v2).xyz;
			float4 tess;

			if (WorldViewFrustumCull(pos0, pos1, pos2, maxDisplacement, planes))
			{
				tess = 0.0f;
			}
			else
			{
				tess.x = CalcEdgeTessFactor (pos1, pos2, edgeLength, cameraPos, scParams);
				tess.y = CalcEdgeTessFactor (pos2, pos0, edgeLength, cameraPos, scParams);
				tess.z = CalcEdgeTessFactor (pos0, pos1, edgeLength, cameraPos, scParams);
				tess.w = (tess.x + tess.y + tess.z) / 3.0f;
			}
			return tess;
		}
		#endif //ASE_TESS_FUNCS
		ENDHLSL

		
		Pass
		{
			
			Name "Forward"
			Tags { "LightMode"="UniversalForwardOnly" }

			Blend One Zero, One Zero
			ZWrite On
			ZTest LEqual
			Offset 0 , 0
			ColorMask RGBA

			

			HLSLPROGRAM

			#pragma multi_compile_instancing
			#pragma instancing_options renderinglayer
			#pragma multi_compile _ LOD_FADE_CROSSFADE
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define ASE_ABSOLUTE_VERTEX_POS 1
			#define _ALPHATEST_ON 1
			#define ASE_SRP_VERSION 140010


			#pragma shader_feature_local _RECEIVE_SHADOWS_OFF
			#pragma multi_compile_fragment _ _SCREEN_SPACE_OCCLUSION
			#pragma multi_compile_fragment _ _DBUFFER_MRT1 _DBUFFER_MRT2 _DBUFFER_MRT3

			

			#pragma multi_compile _ DIRLIGHTMAP_COMBINED
            #pragma multi_compile _ LIGHTMAP_ON
            #pragma multi_compile _ DYNAMICLIGHTMAP_ON
			#pragma multi_compile_fragment _ DEBUG_DISPLAY

			

			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS SHADERPASS_UNLIT

			
            #if ASE_SRP_VERSION >=140007
			#include_with_pragmas "Packages/com.unity.render-pipelines.universal/ShaderLibrary/DOTS.hlsl"
			#endif
		

			
			#if ASE_SRP_VERSION >=140007
			#include_with_pragmas "Packages/com.unity.render-pipelines.universal/ShaderLibrary/RenderingLayers.hlsl"
			#endif
		

			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Texture.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Input.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/TextureStack.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/DBuffer.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/Editor/ShaderGraph/Includes/ShaderPass.hlsl"

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Debug/Debugging3D.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/SurfaceData.hlsl"

			#if defined(LOD_FADE_CROSSFADE)
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/LODCrossFade.hlsl"
            #endif

			#define ASE_NEEDS_VERT_POSITION


			struct VertexInput
			{
				float4 positionOS : POSITION;
				float3 normalOS : NORMAL;
				float4 ase_texcoord : TEXCOORD0;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 positionCS : SV_POSITION;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
					float3 positionWS : TEXCOORD0;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					float4 shadowCoord : TEXCOORD1;
				#endif
				#ifdef ASE_FOG
					float fogFactor : TEXCOORD2;
				#endif
				float4 ase_texcoord3 : TEXCOORD3;
				float4 ase_texcoord4 : TEXCOORD4;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _Color1;
			float4 _TextureSample0_ST;
			float4 _Color0;
			float3 _LocalRotationAxis;
			float _RotationSpeedNoise;
			float _ColorGradientAddToVertexY;
			float _ColorGradient;
			float _ValueMulti;
			float _SaturationMulti;
			float _HueShift;
			float _WaveTexScale;
			float _WaveTexScrollSpeed;
			float _WaveStrengthY;
			float _OffsetFromHoseNoise;
			float _OffsetFromHose;
			float _RadiusLerpExpo;
			float _RadiusBottom;
			float _TwistTop;
			float _TwistStrength;
			float _TwistOffsetLag;
			float _TwistScale;
			float _TwistScrollX;
			float _WrapAroundTwistedHose;
			float _LocalRotationSpeed;
			float _BendStrength;
			float _RotateSlowerOnTop;
			float _RotationSpeed;
			float _RadiusTop;
			float _TexVsColor;
			#ifdef ASE_TESSELLATION
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END

			sampler2D _TwistTexture;
			sampler2D _TextureSample0;


			float3 RotateAroundAxis( float3 center, float3 original, float3 u, float angle )
			{
				original -= center;
				float C = cos( angle );
				float S = sin( angle );
				float t = 1 - C;
				float m00 = t * u.x * u.x + C;
				float m01 = t * u.x * u.y - S * u.z;
				float m02 = t * u.x * u.z + S * u.y;
				float m10 = t * u.x * u.y + S * u.z;
				float m11 = t * u.y * u.y + C;
				float m12 = t * u.y * u.z - S * u.x;
				float m20 = t * u.x * u.z - S * u.y;
				float m21 = t * u.y * u.z + S * u.x;
				float m22 = t * u.z * u.z + C;
				float3x3 finalMatrix = float3x3( m00, m01, m02, m10, m11, m12, m20, m21, m22 );
				return mul( finalMatrix, original ) + center;
			}
			
			float3 mod2D289( float3 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float2 mod2D289( float2 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float3 permute( float3 x ) { return mod2D289( ( ( x * 34.0 ) + 1.0 ) * x ); }
			float snoise( float2 v )
			{
				const float4 C = float4( 0.211324865405187, 0.366025403784439, -0.577350269189626, 0.024390243902439 );
				float2 i = floor( v + dot( v, C.yy ) );
				float2 x0 = v - i + dot( i, C.xx );
				float2 i1;
				i1 = ( x0.x > x0.y ) ? float2( 1.0, 0.0 ) : float2( 0.0, 1.0 );
				float4 x12 = x0.xyxy + C.xxzz;
				x12.xy -= i1;
				i = mod2D289( i );
				float3 p = permute( permute( i.y + float3( 0.0, i1.y, 1.0 ) ) + i.x + float3( 0.0, i1.x, 1.0 ) );
				float3 m = max( 0.5 - float3( dot( x0, x0 ), dot( x12.xy, x12.xy ), dot( x12.zw, x12.zw ) ), 0.0 );
				m = m * m;
				m = m * m;
				float3 x = 2.0 * frac( p * C.www ) - 1.0;
				float3 h = abs( x ) - 0.5;
				float3 ox = floor( x + 0.5 );
				float3 a0 = x - ox;
				m *= 1.79284291400159 - 0.85373472095314 * ( a0 * a0 + h * h );
				float3 g;
				g.x = a0.x * x0.x + h.x * x0.y;
				g.yz = a0.yz * x12.xz + h.yz * x12.yw;
				return 130.0 * dot( m, g );
			}
			
			float3 HSVToRGB( float3 c )
			{
				float4 K = float4( 1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0 );
				float3 p = abs( frac( c.xxx + K.xyz ) * 6.0 - K.www );
				return c.z * lerp( K.xxx, saturate( p - K.xxx ), c.y );
			}
			
			float3 RGBToHSV(float3 c)
			{
				float4 K = float4(0.0, -1.0 / 3.0, 2.0 / 3.0, -1.0);
				float4 p = lerp( float4( c.bg, K.wz ), float4( c.gb, K.xy ), step( c.b, c.g ) );
				float4 q = lerp( float4( p.xyw, c.r ), float4( c.r, p.yzx ), step( p.x, c.r ) );
				float d = q.x - min( q.w, q.y );
				float e = 1.0e-10;
				return float3( abs(q.z + (q.w - q.y) / (6.0 * d + e)), d / (q.x + e), q.x);
			}

			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				float4 transform69 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_0 = (transform69.y).xx;
				float dotResult4_g1 = dot( temp_cast_0 , float2( 12.9898,78.233 ) );
				float lerpResult10_g1 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g1 ) * 43758.55 ) ));
				float temp_output_70_0 = lerpResult10_g1;
				float2 temp_cast_1 = (temp_output_70_0).xx;
				float dotResult4_g2 = dot( temp_cast_1 , float2( 12.9898,78.233 ) );
				float lerpResult10_g2 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g2 ) * 43758.55 ) ));
				float temp_output_93_0 = lerpResult10_g2;
				float RandomViaY0296 = temp_output_93_0;
				float4 transform64 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float TornadoHeight60 = ( transform64.y / ( 2.0 * PI ) );
				float mulTime34 = _TimeParameters.x * _LocalRotationSpeed;
				float2 temp_cast_2 = (temp_output_93_0).xx;
				float dotResult4_g3 = dot( temp_cast_2 , float2( 12.9898,78.233 ) );
				float lerpResult10_g3 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g3 ) * 43758.55 ) ));
				float temp_output_106_0 = lerpResult10_g3;
				float2 temp_cast_3 = (temp_output_106_0).xx;
				float dotResult4_g4 = dot( temp_cast_3 , float2( 12.9898,78.233 ) );
				float lerpResult10_g4 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g4 ) * 43758.55 ) ));
				float RandomViaY03149 = lerpResult10_g4;
				float3 rotatedValue31 = RotateAroundAxis( float3( 0,0,0 ), v.positionOS.xyz, normalize( _LocalRotationAxis ), ( mulTime34 + ( RandomViaY03149 * 100.0 ) ) );
				float3 rotatedValue22 = RotateAroundAxis( float3( 0,0,0 ), rotatedValue31, float3( 1,0,0 ), ( _BendStrength * sign( rotatedValue31.z ) * abs( rotatedValue31.z ) ) );
				float3 objToWorld16 = mul( GetObjectToWorldMatrix(), float4( rotatedValue22, 1 ) ).xyz;
				float3 LocallyBendAndRotatedToWorld140 = objToWorld16;
				float temp_output_144_0 = ( LocallyBendAndRotatedToWorld140.y / ( 2.0 * PI ) );
				float lerpResult146 = lerp( temp_output_144_0 , TornadoHeight60 , _WrapAroundTwistedHose);
				float mulTime41 = _TimeParameters.x * _TwistScrollX;
				float2 temp_cast_4 = (( ( ( lerpResult146 - mulTime41 ) * _TwistScale ) + ( _TwistOffsetLag * ( 1.0 + RandomViaY03149 ) ) )).xx;
				float temp_output_54_0 = sin( ( ( _TwistTop * lerpResult146 ) * PI ) );
				float2 temp_cast_5 = (( ( ( lerpResult146 - mulTime41 ) * _TwistScale ) + ( _TwistOffsetLag * ( 1.0 + RandomViaY03149 ) ) )).xx;
				float3 appendResult58 = (float3(( sin( ( ( tex2Dlod( _TwistTexture, float4( temp_cast_4, 0, 0.0) ).r * 2.0 ) * PI ) ) * _TwistStrength * temp_output_54_0 ) , 0.0 , ( cos( ( ( tex2Dlod( _TwistTexture, float4( temp_cast_4, 0, 0.0) ).r * 2.0 ) * PI ) ) * _TwistStrength * temp_output_54_0 )));
				float3 TwistOffset59 = appendResult58;
				float lerpResult122 = lerp( _RadiusBottom , _RadiusTop , pow( temp_output_144_0 , _RadiusLerpExpo ));
				float3 appendResult127 = (float3(lerpResult122 , 0.0 , 0.0));
				float3 break26 = objToWorld16;
				float3 appendResult15 = (float3(( break26.x + ( _OffsetFromHose + ( _OffsetFromHose * _OffsetFromHoseNoise * temp_output_106_0 ) ) ) , break26.y , break26.z));
				float3 rotatedValue18 = RotateAroundAxis( TwistOffset59, ( TwistOffset59 + ( appendResult127 + appendResult15 ) ), float3( 0,1,0 ), ( ( _TimeParameters.x * ( ( ( -_RotationSpeedNoise * RandomViaY0296 ) + 1.0 ) * _RotationSpeed ) * ( 1.0 + ( -_RotateSlowerOnTop * saturate( TornadoHeight60 ) ) ) ) + ( ( 2.0 * PI ) * RandomViaY0296 ) ) );
				float2 temp_cast_6 = (( ( ( _TimeParameters.x * _WaveTexScrollSpeed ) + ( temp_output_70_0 * 100.0 ) ) * _WaveTexScale )).xx;
				float simplePerlin2D108 = snoise( temp_cast_6*_WaveTexScale );
				simplePerlin2D108 = simplePerlin2D108*0.5 + 0.5;
				float3 appendResult86 = (float3(0.0 , ( _WaveStrengthY * simplePerlin2D108 ) , 0.0));
				float3 WaveOffset85 = appendResult86;
				float3 worldToObj17 = mul( GetWorldToObjectMatrix(), float4( ( rotatedValue18 + WaveOffset85 ), 1 ) ).xyz;
				
				o.ase_texcoord3.xy = v.ase_texcoord.xy;
				o.ase_texcoord4 = v.positionOS;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord3.zw = 0;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.positionOS.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif

				float3 vertexValue = worldToObj17;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.positionOS.xyz = vertexValue;
				#else
					v.positionOS.xyz += vertexValue;
				#endif

				v.normalOS = v.normalOS;

				float3 positionWS = TransformObjectToWorld( v.positionOS.xyz );
				float4 positionCS = TransformWorldToHClip( positionWS );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
					o.positionWS = positionWS;
				#endif

				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					VertexPositionInputs vertexInput = (VertexPositionInputs)0;
					vertexInput.positionWS = positionWS;
					vertexInput.positionCS = positionCS;
					o.shadowCoord = GetShadowCoord( vertexInput );
				#endif

				#ifdef ASE_FOG
					o.fogFactor = ComputeFogFactor( positionCS.z );
				#endif

				o.positionCS = positionCS;

				return o;
			}

			#if defined(ASE_TESSELLATION)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 normalOS : NORMAL;
				float4 ase_texcoord : TEXCOORD0;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.positionOS;
				o.normalOS = v.normalOS;
				o.ase_texcoord = v.ase_texcoord;
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
				return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.positionOS = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.normalOS = patch[0].normalOS * bary.x + patch[1].normalOS * bary.y + patch[2].normalOS * bary.z;
				o.ase_texcoord = patch[0].ase_texcoord * bary.x + patch[1].ase_texcoord * bary.y + patch[2].ase_texcoord * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.positionOS.xyz - patch[i].normalOS * (dot(o.positionOS.xyz, patch[i].normalOS) - dot(patch[i].vertex.xyz, patch[i].normalOS));
				float phongStrength = _TessPhongStrength;
				o.positionOS.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.positionOS.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			half4 frag ( VertexOutput IN
				#ifdef _WRITE_RENDERING_LAYERS
				, out float4 outRenderingLayers : SV_Target1
				#endif
				 ) : SV_Target
			{
				UNITY_SETUP_INSTANCE_ID( IN );
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( IN );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
					float3 WorldPosition = IN.positionWS;
				#endif

				float4 ShadowCoords = float4( 0, 0, 0, 0 );

				#if defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
						ShadowCoords = IN.shadowCoord;
					#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
						ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
					#endif
				#endif

				float2 uv_TextureSample0 = IN.ase_texcoord3.xy * _TextureSample0_ST.xy + _TextureSample0_ST.zw;
				float4 tex2DNode10 = tex2D( _TextureSample0, uv_TextureSample0 );
				float3 hsvTorgb180 = RGBToHSV( tex2DNode10.rgb );
				float3 hsvTorgb182 = HSVToRGB( float3(( _HueShift + hsvTorgb180.x ),( _SaturationMulti * hsvTorgb180.y ),( hsvTorgb180.z * _ValueMulti )) );
				float4 transform69 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_2 = (transform69.y).xx;
				float dotResult4_g1 = dot( temp_cast_2 , float2( 12.9898,78.233 ) );
				float lerpResult10_g1 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g1 ) * 43758.55 ) ));
				float temp_output_70_0 = lerpResult10_g1;
				float RandomViaY01g97 = temp_output_70_0;
				float4 lerpResult92 = lerp( _Color1 , _Color0 , RandomViaY01g97);
				float4 lerpResult99 = lerp( float4( hsvTorgb182 , 0.0 ) , ( _ColorGradient * ( _ColorGradientAddToVertexY + IN.ase_texcoord4.xyz.y ) * lerpResult92 ) , _TexVsColor);
				
				float lerpResult100 = lerp( tex2DNode10.a , 1.0 , _TexVsColor);
				
				float3 BakedAlbedo = 0;
				float3 BakedEmission = 0;
				float3 Color = lerpResult99.rgb;
				float Alpha = lerpResult100;
				float AlphaClipThreshold = 0.1;
				float AlphaClipThresholdShadow = 0.5;

				#ifdef _ALPHATEST_ON
					clip( Alpha - AlphaClipThreshold );
				#endif

				#if defined(_DBUFFER)
					ApplyDecalToBaseColor(IN.positionCS, Color);
				#endif

				#if defined(_ALPHAPREMULTIPLY_ON)
				Color *= Alpha;
				#endif

				#ifdef LOD_FADE_CROSSFADE
					LODFadeCrossFade( IN.positionCS );
				#endif

				#ifdef ASE_FOG
					Color = MixFog( Color, IN.fogFactor );
				#endif

				#ifdef _WRITE_RENDERING_LAYERS
					uint renderingLayers = GetMeshRenderingLayer();
					outRenderingLayers = float4( EncodeMeshRenderingLayer( renderingLayers ), 0, 0, 0 );
				#endif

				return half4( Color, Alpha );
			}
			ENDHLSL
		}

		
		Pass
		{
			
			Name "ShadowCaster"
			Tags { "LightMode"="ShadowCaster" }

			ZWrite On
			ZTest LEqual
			AlphaToMask Off
			ColorMask 0

			HLSLPROGRAM

			#pragma multi_compile_instancing
			#pragma multi_compile _ LOD_FADE_CROSSFADE
			#define ASE_FOG 1
			#define ASE_ABSOLUTE_VERTEX_POS 1
			#define _ALPHATEST_ON 1
			#define ASE_SRP_VERSION 140010


			#pragma vertex vert
			#pragma fragment frag

			#pragma multi_compile_vertex _ _CASTING_PUNCTUAL_LIGHT_SHADOW

			

			#define SHADERPASS SHADERPASS_SHADOWCASTER

			
            #if ASE_SRP_VERSION >=140007
			#include_with_pragmas "Packages/com.unity.render-pipelines.universal/ShaderLibrary/DOTS.hlsl"
			#endif
		

			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"

			#if defined(LOD_FADE_CROSSFADE)
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/LODCrossFade.hlsl"
            #endif

			#define ASE_NEEDS_VERT_POSITION


			struct VertexInput
			{
				float4 positionOS : POSITION;
				float3 normalOS : NORMAL;
				float4 ase_texcoord : TEXCOORD0;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 positionCS : SV_POSITION;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
					float3 positionWS : TEXCOORD0;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					float4 shadowCoord : TEXCOORD1;
				#endif
				float4 ase_texcoord2 : TEXCOORD2;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _Color1;
			float4 _TextureSample0_ST;
			float4 _Color0;
			float3 _LocalRotationAxis;
			float _RotationSpeedNoise;
			float _ColorGradientAddToVertexY;
			float _ColorGradient;
			float _ValueMulti;
			float _SaturationMulti;
			float _HueShift;
			float _WaveTexScale;
			float _WaveTexScrollSpeed;
			float _WaveStrengthY;
			float _OffsetFromHoseNoise;
			float _OffsetFromHose;
			float _RadiusLerpExpo;
			float _RadiusBottom;
			float _TwistTop;
			float _TwistStrength;
			float _TwistOffsetLag;
			float _TwistScale;
			float _TwistScrollX;
			float _WrapAroundTwistedHose;
			float _LocalRotationSpeed;
			float _BendStrength;
			float _RotateSlowerOnTop;
			float _RotationSpeed;
			float _RadiusTop;
			float _TexVsColor;
			#ifdef ASE_TESSELLATION
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END

			sampler2D _TwistTexture;
			sampler2D _TextureSample0;


			float3 RotateAroundAxis( float3 center, float3 original, float3 u, float angle )
			{
				original -= center;
				float C = cos( angle );
				float S = sin( angle );
				float t = 1 - C;
				float m00 = t * u.x * u.x + C;
				float m01 = t * u.x * u.y - S * u.z;
				float m02 = t * u.x * u.z + S * u.y;
				float m10 = t * u.x * u.y + S * u.z;
				float m11 = t * u.y * u.y + C;
				float m12 = t * u.y * u.z - S * u.x;
				float m20 = t * u.x * u.z - S * u.y;
				float m21 = t * u.y * u.z + S * u.x;
				float m22 = t * u.z * u.z + C;
				float3x3 finalMatrix = float3x3( m00, m01, m02, m10, m11, m12, m20, m21, m22 );
				return mul( finalMatrix, original ) + center;
			}
			
			float3 mod2D289( float3 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float2 mod2D289( float2 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float3 permute( float3 x ) { return mod2D289( ( ( x * 34.0 ) + 1.0 ) * x ); }
			float snoise( float2 v )
			{
				const float4 C = float4( 0.211324865405187, 0.366025403784439, -0.577350269189626, 0.024390243902439 );
				float2 i = floor( v + dot( v, C.yy ) );
				float2 x0 = v - i + dot( i, C.xx );
				float2 i1;
				i1 = ( x0.x > x0.y ) ? float2( 1.0, 0.0 ) : float2( 0.0, 1.0 );
				float4 x12 = x0.xyxy + C.xxzz;
				x12.xy -= i1;
				i = mod2D289( i );
				float3 p = permute( permute( i.y + float3( 0.0, i1.y, 1.0 ) ) + i.x + float3( 0.0, i1.x, 1.0 ) );
				float3 m = max( 0.5 - float3( dot( x0, x0 ), dot( x12.xy, x12.xy ), dot( x12.zw, x12.zw ) ), 0.0 );
				m = m * m;
				m = m * m;
				float3 x = 2.0 * frac( p * C.www ) - 1.0;
				float3 h = abs( x ) - 0.5;
				float3 ox = floor( x + 0.5 );
				float3 a0 = x - ox;
				m *= 1.79284291400159 - 0.85373472095314 * ( a0 * a0 + h * h );
				float3 g;
				g.x = a0.x * x0.x + h.x * x0.y;
				g.yz = a0.yz * x12.xz + h.yz * x12.yw;
				return 130.0 * dot( m, g );
			}
			

			float3 _LightDirection;
			float3 _LightPosition;

			VertexOutput VertexFunction( VertexInput v )
			{
				VertexOutput o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO( o );

				float4 transform69 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_0 = (transform69.y).xx;
				float dotResult4_g1 = dot( temp_cast_0 , float2( 12.9898,78.233 ) );
				float lerpResult10_g1 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g1 ) * 43758.55 ) ));
				float temp_output_70_0 = lerpResult10_g1;
				float2 temp_cast_1 = (temp_output_70_0).xx;
				float dotResult4_g2 = dot( temp_cast_1 , float2( 12.9898,78.233 ) );
				float lerpResult10_g2 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g2 ) * 43758.55 ) ));
				float temp_output_93_0 = lerpResult10_g2;
				float RandomViaY0296 = temp_output_93_0;
				float4 transform64 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float TornadoHeight60 = ( transform64.y / ( 2.0 * PI ) );
				float mulTime34 = _TimeParameters.x * _LocalRotationSpeed;
				float2 temp_cast_2 = (temp_output_93_0).xx;
				float dotResult4_g3 = dot( temp_cast_2 , float2( 12.9898,78.233 ) );
				float lerpResult10_g3 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g3 ) * 43758.55 ) ));
				float temp_output_106_0 = lerpResult10_g3;
				float2 temp_cast_3 = (temp_output_106_0).xx;
				float dotResult4_g4 = dot( temp_cast_3 , float2( 12.9898,78.233 ) );
				float lerpResult10_g4 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g4 ) * 43758.55 ) ));
				float RandomViaY03149 = lerpResult10_g4;
				float3 rotatedValue31 = RotateAroundAxis( float3( 0,0,0 ), v.positionOS.xyz, normalize( _LocalRotationAxis ), ( mulTime34 + ( RandomViaY03149 * 100.0 ) ) );
				float3 rotatedValue22 = RotateAroundAxis( float3( 0,0,0 ), rotatedValue31, float3( 1,0,0 ), ( _BendStrength * sign( rotatedValue31.z ) * abs( rotatedValue31.z ) ) );
				float3 objToWorld16 = mul( GetObjectToWorldMatrix(), float4( rotatedValue22, 1 ) ).xyz;
				float3 LocallyBendAndRotatedToWorld140 = objToWorld16;
				float temp_output_144_0 = ( LocallyBendAndRotatedToWorld140.y / ( 2.0 * PI ) );
				float lerpResult146 = lerp( temp_output_144_0 , TornadoHeight60 , _WrapAroundTwistedHose);
				float mulTime41 = _TimeParameters.x * _TwistScrollX;
				float2 temp_cast_4 = (( ( ( lerpResult146 - mulTime41 ) * _TwistScale ) + ( _TwistOffsetLag * ( 1.0 + RandomViaY03149 ) ) )).xx;
				float temp_output_54_0 = sin( ( ( _TwistTop * lerpResult146 ) * PI ) );
				float2 temp_cast_5 = (( ( ( lerpResult146 - mulTime41 ) * _TwistScale ) + ( _TwistOffsetLag * ( 1.0 + RandomViaY03149 ) ) )).xx;
				float3 appendResult58 = (float3(( sin( ( ( tex2Dlod( _TwistTexture, float4( temp_cast_4, 0, 0.0) ).r * 2.0 ) * PI ) ) * _TwistStrength * temp_output_54_0 ) , 0.0 , ( cos( ( ( tex2Dlod( _TwistTexture, float4( temp_cast_4, 0, 0.0) ).r * 2.0 ) * PI ) ) * _TwistStrength * temp_output_54_0 )));
				float3 TwistOffset59 = appendResult58;
				float lerpResult122 = lerp( _RadiusBottom , _RadiusTop , pow( temp_output_144_0 , _RadiusLerpExpo ));
				float3 appendResult127 = (float3(lerpResult122 , 0.0 , 0.0));
				float3 break26 = objToWorld16;
				float3 appendResult15 = (float3(( break26.x + ( _OffsetFromHose + ( _OffsetFromHose * _OffsetFromHoseNoise * temp_output_106_0 ) ) ) , break26.y , break26.z));
				float3 rotatedValue18 = RotateAroundAxis( TwistOffset59, ( TwistOffset59 + ( appendResult127 + appendResult15 ) ), float3( 0,1,0 ), ( ( _TimeParameters.x * ( ( ( -_RotationSpeedNoise * RandomViaY0296 ) + 1.0 ) * _RotationSpeed ) * ( 1.0 + ( -_RotateSlowerOnTop * saturate( TornadoHeight60 ) ) ) ) + ( ( 2.0 * PI ) * RandomViaY0296 ) ) );
				float2 temp_cast_6 = (( ( ( _TimeParameters.x * _WaveTexScrollSpeed ) + ( temp_output_70_0 * 100.0 ) ) * _WaveTexScale )).xx;
				float simplePerlin2D108 = snoise( temp_cast_6*_WaveTexScale );
				simplePerlin2D108 = simplePerlin2D108*0.5 + 0.5;
				float3 appendResult86 = (float3(0.0 , ( _WaveStrengthY * simplePerlin2D108 ) , 0.0));
				float3 WaveOffset85 = appendResult86;
				float3 worldToObj17 = mul( GetWorldToObjectMatrix(), float4( ( rotatedValue18 + WaveOffset85 ), 1 ) ).xyz;
				
				o.ase_texcoord2.xy = v.ase_texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord2.zw = 0;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.positionOS.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif

				float3 vertexValue = worldToObj17;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.positionOS.xyz = vertexValue;
				#else
					v.positionOS.xyz += vertexValue;
				#endif

				v.normalOS = v.normalOS;

				float3 positionWS = TransformObjectToWorld( v.positionOS.xyz );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
					o.positionWS = positionWS;
				#endif

				float3 normalWS = TransformObjectToWorldDir( v.normalOS );

				#if _CASTING_PUNCTUAL_LIGHT_SHADOW
					float3 lightDirectionWS = normalize(_LightPosition - positionWS);
				#else
					float3 lightDirectionWS = _LightDirection;
				#endif

				float4 positionCS = TransformWorldToHClip(ApplyShadowBias(positionWS, normalWS, lightDirectionWS));

				#if UNITY_REVERSED_Z
					positionCS.z = min(positionCS.z, UNITY_NEAR_CLIP_VALUE);
				#else
					positionCS.z = max(positionCS.z, UNITY_NEAR_CLIP_VALUE);
				#endif

				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					VertexPositionInputs vertexInput = (VertexPositionInputs)0;
					vertexInput.positionWS = positionWS;
					vertexInput.positionCS = positionCS;
					o.shadowCoord = GetShadowCoord( vertexInput );
				#endif

				o.positionCS = positionCS;

				return o;
			}

			#if defined(ASE_TESSELLATION)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 normalOS : NORMAL;
				float4 ase_texcoord : TEXCOORD0;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.positionOS;
				o.normalOS = v.normalOS;
				o.ase_texcoord = v.ase_texcoord;
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
				return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.positionOS = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.normalOS = patch[0].normalOS * bary.x + patch[1].normalOS * bary.y + patch[2].normalOS * bary.z;
				o.ase_texcoord = patch[0].ase_texcoord * bary.x + patch[1].ase_texcoord * bary.y + patch[2].ase_texcoord * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.positionOS.xyz - patch[i].normalOS * (dot(o.positionOS.xyz, patch[i].normalOS) - dot(patch[i].vertex.xyz, patch[i].normalOS));
				float phongStrength = _TessPhongStrength;
				o.positionOS.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.positionOS.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			half4 frag(VertexOutput IN  ) : SV_TARGET
			{
				UNITY_SETUP_INSTANCE_ID( IN );
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( IN );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
					float3 WorldPosition = IN.positionWS;
				#endif

				float4 ShadowCoords = float4( 0, 0, 0, 0 );

				#if defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
						ShadowCoords = IN.shadowCoord;
					#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
						ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
					#endif
				#endif

				float2 uv_TextureSample0 = IN.ase_texcoord2.xy * _TextureSample0_ST.xy + _TextureSample0_ST.zw;
				float4 tex2DNode10 = tex2D( _TextureSample0, uv_TextureSample0 );
				float lerpResult100 = lerp( tex2DNode10.a , 1.0 , _TexVsColor);
				

				float Alpha = lerpResult100;
				float AlphaClipThreshold = 0.1;
				float AlphaClipThresholdShadow = 0.5;

				#ifdef _ALPHATEST_ON
					#ifdef _ALPHATEST_SHADOW_ON
						clip(Alpha - AlphaClipThresholdShadow);
					#else
						clip(Alpha - AlphaClipThreshold);
					#endif
				#endif

				#ifdef LOD_FADE_CROSSFADE
					LODFadeCrossFade( IN.positionCS );
				#endif

				return 0;
			}
			ENDHLSL
		}

		
		Pass
		{
			
			Name "DepthOnly"
			Tags { "LightMode"="DepthOnly" }

			ZWrite On
			ColorMask R
			AlphaToMask Off

			HLSLPROGRAM

			

			#pragma multi_compile_instancing
			#pragma multi_compile _ LOD_FADE_CROSSFADE
			#define ASE_FOG 1
			#define ASE_ABSOLUTE_VERTEX_POS 1
			#define _ALPHATEST_ON 1
			#define ASE_SRP_VERSION 140010


			

			#pragma vertex vert
			#pragma fragment frag

			
            #if ASE_SRP_VERSION >=140007
			#include_with_pragmas "Packages/com.unity.render-pipelines.universal/ShaderLibrary/DOTS.hlsl"
			#endif
		

			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"

			#if defined(LOD_FADE_CROSSFADE)
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/LODCrossFade.hlsl"
            #endif

			#define ASE_NEEDS_VERT_POSITION


			struct VertexInput
			{
				float4 positionOS : POSITION;
				float3 normalOS : NORMAL;
				float4 ase_texcoord : TEXCOORD0;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 positionCS : SV_POSITION;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 positionWS : TEXCOORD0;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
				float4 shadowCoord : TEXCOORD1;
				#endif
				float4 ase_texcoord2 : TEXCOORD2;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _Color1;
			float4 _TextureSample0_ST;
			float4 _Color0;
			float3 _LocalRotationAxis;
			float _RotationSpeedNoise;
			float _ColorGradientAddToVertexY;
			float _ColorGradient;
			float _ValueMulti;
			float _SaturationMulti;
			float _HueShift;
			float _WaveTexScale;
			float _WaveTexScrollSpeed;
			float _WaveStrengthY;
			float _OffsetFromHoseNoise;
			float _OffsetFromHose;
			float _RadiusLerpExpo;
			float _RadiusBottom;
			float _TwistTop;
			float _TwistStrength;
			float _TwistOffsetLag;
			float _TwistScale;
			float _TwistScrollX;
			float _WrapAroundTwistedHose;
			float _LocalRotationSpeed;
			float _BendStrength;
			float _RotateSlowerOnTop;
			float _RotationSpeed;
			float _RadiusTop;
			float _TexVsColor;
			#ifdef ASE_TESSELLATION
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END

			sampler2D _TwistTexture;
			sampler2D _TextureSample0;


			float3 RotateAroundAxis( float3 center, float3 original, float3 u, float angle )
			{
				original -= center;
				float C = cos( angle );
				float S = sin( angle );
				float t = 1 - C;
				float m00 = t * u.x * u.x + C;
				float m01 = t * u.x * u.y - S * u.z;
				float m02 = t * u.x * u.z + S * u.y;
				float m10 = t * u.x * u.y + S * u.z;
				float m11 = t * u.y * u.y + C;
				float m12 = t * u.y * u.z - S * u.x;
				float m20 = t * u.x * u.z - S * u.y;
				float m21 = t * u.y * u.z + S * u.x;
				float m22 = t * u.z * u.z + C;
				float3x3 finalMatrix = float3x3( m00, m01, m02, m10, m11, m12, m20, m21, m22 );
				return mul( finalMatrix, original ) + center;
			}
			
			float3 mod2D289( float3 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float2 mod2D289( float2 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float3 permute( float3 x ) { return mod2D289( ( ( x * 34.0 ) + 1.0 ) * x ); }
			float snoise( float2 v )
			{
				const float4 C = float4( 0.211324865405187, 0.366025403784439, -0.577350269189626, 0.024390243902439 );
				float2 i = floor( v + dot( v, C.yy ) );
				float2 x0 = v - i + dot( i, C.xx );
				float2 i1;
				i1 = ( x0.x > x0.y ) ? float2( 1.0, 0.0 ) : float2( 0.0, 1.0 );
				float4 x12 = x0.xyxy + C.xxzz;
				x12.xy -= i1;
				i = mod2D289( i );
				float3 p = permute( permute( i.y + float3( 0.0, i1.y, 1.0 ) ) + i.x + float3( 0.0, i1.x, 1.0 ) );
				float3 m = max( 0.5 - float3( dot( x0, x0 ), dot( x12.xy, x12.xy ), dot( x12.zw, x12.zw ) ), 0.0 );
				m = m * m;
				m = m * m;
				float3 x = 2.0 * frac( p * C.www ) - 1.0;
				float3 h = abs( x ) - 0.5;
				float3 ox = floor( x + 0.5 );
				float3 a0 = x - ox;
				m *= 1.79284291400159 - 0.85373472095314 * ( a0 * a0 + h * h );
				float3 g;
				g.x = a0.x * x0.x + h.x * x0.y;
				g.yz = a0.yz * x12.xz + h.yz * x12.yw;
				return 130.0 * dot( m, g );
			}
			

			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				float4 transform69 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_0 = (transform69.y).xx;
				float dotResult4_g1 = dot( temp_cast_0 , float2( 12.9898,78.233 ) );
				float lerpResult10_g1 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g1 ) * 43758.55 ) ));
				float temp_output_70_0 = lerpResult10_g1;
				float2 temp_cast_1 = (temp_output_70_0).xx;
				float dotResult4_g2 = dot( temp_cast_1 , float2( 12.9898,78.233 ) );
				float lerpResult10_g2 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g2 ) * 43758.55 ) ));
				float temp_output_93_0 = lerpResult10_g2;
				float RandomViaY0296 = temp_output_93_0;
				float4 transform64 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float TornadoHeight60 = ( transform64.y / ( 2.0 * PI ) );
				float mulTime34 = _TimeParameters.x * _LocalRotationSpeed;
				float2 temp_cast_2 = (temp_output_93_0).xx;
				float dotResult4_g3 = dot( temp_cast_2 , float2( 12.9898,78.233 ) );
				float lerpResult10_g3 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g3 ) * 43758.55 ) ));
				float temp_output_106_0 = lerpResult10_g3;
				float2 temp_cast_3 = (temp_output_106_0).xx;
				float dotResult4_g4 = dot( temp_cast_3 , float2( 12.9898,78.233 ) );
				float lerpResult10_g4 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g4 ) * 43758.55 ) ));
				float RandomViaY03149 = lerpResult10_g4;
				float3 rotatedValue31 = RotateAroundAxis( float3( 0,0,0 ), v.positionOS.xyz, normalize( _LocalRotationAxis ), ( mulTime34 + ( RandomViaY03149 * 100.0 ) ) );
				float3 rotatedValue22 = RotateAroundAxis( float3( 0,0,0 ), rotatedValue31, float3( 1,0,0 ), ( _BendStrength * sign( rotatedValue31.z ) * abs( rotatedValue31.z ) ) );
				float3 objToWorld16 = mul( GetObjectToWorldMatrix(), float4( rotatedValue22, 1 ) ).xyz;
				float3 LocallyBendAndRotatedToWorld140 = objToWorld16;
				float temp_output_144_0 = ( LocallyBendAndRotatedToWorld140.y / ( 2.0 * PI ) );
				float lerpResult146 = lerp( temp_output_144_0 , TornadoHeight60 , _WrapAroundTwistedHose);
				float mulTime41 = _TimeParameters.x * _TwistScrollX;
				float2 temp_cast_4 = (( ( ( lerpResult146 - mulTime41 ) * _TwistScale ) + ( _TwistOffsetLag * ( 1.0 + RandomViaY03149 ) ) )).xx;
				float temp_output_54_0 = sin( ( ( _TwistTop * lerpResult146 ) * PI ) );
				float2 temp_cast_5 = (( ( ( lerpResult146 - mulTime41 ) * _TwistScale ) + ( _TwistOffsetLag * ( 1.0 + RandomViaY03149 ) ) )).xx;
				float3 appendResult58 = (float3(( sin( ( ( tex2Dlod( _TwistTexture, float4( temp_cast_4, 0, 0.0) ).r * 2.0 ) * PI ) ) * _TwistStrength * temp_output_54_0 ) , 0.0 , ( cos( ( ( tex2Dlod( _TwistTexture, float4( temp_cast_4, 0, 0.0) ).r * 2.0 ) * PI ) ) * _TwistStrength * temp_output_54_0 )));
				float3 TwistOffset59 = appendResult58;
				float lerpResult122 = lerp( _RadiusBottom , _RadiusTop , pow( temp_output_144_0 , _RadiusLerpExpo ));
				float3 appendResult127 = (float3(lerpResult122 , 0.0 , 0.0));
				float3 break26 = objToWorld16;
				float3 appendResult15 = (float3(( break26.x + ( _OffsetFromHose + ( _OffsetFromHose * _OffsetFromHoseNoise * temp_output_106_0 ) ) ) , break26.y , break26.z));
				float3 rotatedValue18 = RotateAroundAxis( TwistOffset59, ( TwistOffset59 + ( appendResult127 + appendResult15 ) ), float3( 0,1,0 ), ( ( _TimeParameters.x * ( ( ( -_RotationSpeedNoise * RandomViaY0296 ) + 1.0 ) * _RotationSpeed ) * ( 1.0 + ( -_RotateSlowerOnTop * saturate( TornadoHeight60 ) ) ) ) + ( ( 2.0 * PI ) * RandomViaY0296 ) ) );
				float2 temp_cast_6 = (( ( ( _TimeParameters.x * _WaveTexScrollSpeed ) + ( temp_output_70_0 * 100.0 ) ) * _WaveTexScale )).xx;
				float simplePerlin2D108 = snoise( temp_cast_6*_WaveTexScale );
				simplePerlin2D108 = simplePerlin2D108*0.5 + 0.5;
				float3 appendResult86 = (float3(0.0 , ( _WaveStrengthY * simplePerlin2D108 ) , 0.0));
				float3 WaveOffset85 = appendResult86;
				float3 worldToObj17 = mul( GetWorldToObjectMatrix(), float4( ( rotatedValue18 + WaveOffset85 ), 1 ) ).xyz;
				
				o.ase_texcoord2.xy = v.ase_texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord2.zw = 0;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.positionOS.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif

				float3 vertexValue = worldToObj17;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.positionOS.xyz = vertexValue;
				#else
					v.positionOS.xyz += vertexValue;
				#endif

				v.normalOS = v.normalOS;

				float3 positionWS = TransformObjectToWorld( v.positionOS.xyz );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
					o.positionWS = positionWS;
				#endif

				o.positionCS = TransformWorldToHClip( positionWS );
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					VertexPositionInputs vertexInput = (VertexPositionInputs)0;
					vertexInput.positionWS = positionWS;
					vertexInput.positionCS = o.positionCS;
					o.shadowCoord = GetShadowCoord( vertexInput );
				#endif

				return o;
			}

			#if defined(ASE_TESSELLATION)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 normalOS : NORMAL;
				float4 ase_texcoord : TEXCOORD0;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.positionOS;
				o.normalOS = v.normalOS;
				o.ase_texcoord = v.ase_texcoord;
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
				return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.positionOS = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.normalOS = patch[0].normalOS * bary.x + patch[1].normalOS * bary.y + patch[2].normalOS * bary.z;
				o.ase_texcoord = patch[0].ase_texcoord * bary.x + patch[1].ase_texcoord * bary.y + patch[2].ase_texcoord * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.positionOS.xyz - patch[i].normalOS * (dot(o.positionOS.xyz, patch[i].normalOS) - dot(patch[i].vertex.xyz, patch[i].normalOS));
				float phongStrength = _TessPhongStrength;
				o.positionOS.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.positionOS.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			half4 frag(VertexOutput IN  ) : SV_TARGET
			{
				UNITY_SETUP_INSTANCE_ID(IN);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( IN );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 WorldPosition = IN.positionWS;
				#endif

				float4 ShadowCoords = float4( 0, 0, 0, 0 );

				#if defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
						ShadowCoords = IN.shadowCoord;
					#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
						ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
					#endif
				#endif

				float2 uv_TextureSample0 = IN.ase_texcoord2.xy * _TextureSample0_ST.xy + _TextureSample0_ST.zw;
				float4 tex2DNode10 = tex2D( _TextureSample0, uv_TextureSample0 );
				float lerpResult100 = lerp( tex2DNode10.a , 1.0 , _TexVsColor);
				

				float Alpha = lerpResult100;
				float AlphaClipThreshold = 0.1;

				#ifdef _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				#ifdef LOD_FADE_CROSSFADE
					LODFadeCrossFade( IN.positionCS );
				#endif
				return 0;
			}
			ENDHLSL
		}

		
		Pass
		{
			
			Name "SceneSelectionPass"
			Tags { "LightMode"="SceneSelectionPass" }

			Cull Off
			AlphaToMask Off

			HLSLPROGRAM

			

			#define ASE_FOG 1
			#define ASE_ABSOLUTE_VERTEX_POS 1
			#define _ALPHATEST_ON 1
			#define ASE_SRP_VERSION 140010


			

			#pragma vertex vert
			#pragma fragment frag

			#define ATTRIBUTES_NEED_NORMAL
			#define ATTRIBUTES_NEED_TANGENT
			#define SHADERPASS SHADERPASS_DEPTHONLY

			
            #if ASE_SRP_VERSION >=140007
			#include_with_pragmas "Packages/com.unity.render-pipelines.universal/ShaderLibrary/DOTS.hlsl"
			#endif
		

			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Texture.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/TextureStack.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/Editor/ShaderGraph/Includes/ShaderPass.hlsl"

			#define ASE_NEEDS_VERT_POSITION


			struct VertexInput
			{
				float4 positionOS : POSITION;
				float3 normalOS : NORMAL;
				float4 ase_texcoord : TEXCOORD0;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 positionCS : SV_POSITION;
				float4 ase_texcoord : TEXCOORD0;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _Color1;
			float4 _TextureSample0_ST;
			float4 _Color0;
			float3 _LocalRotationAxis;
			float _RotationSpeedNoise;
			float _ColorGradientAddToVertexY;
			float _ColorGradient;
			float _ValueMulti;
			float _SaturationMulti;
			float _HueShift;
			float _WaveTexScale;
			float _WaveTexScrollSpeed;
			float _WaveStrengthY;
			float _OffsetFromHoseNoise;
			float _OffsetFromHose;
			float _RadiusLerpExpo;
			float _RadiusBottom;
			float _TwistTop;
			float _TwistStrength;
			float _TwistOffsetLag;
			float _TwistScale;
			float _TwistScrollX;
			float _WrapAroundTwistedHose;
			float _LocalRotationSpeed;
			float _BendStrength;
			float _RotateSlowerOnTop;
			float _RotationSpeed;
			float _RadiusTop;
			float _TexVsColor;
			#ifdef ASE_TESSELLATION
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END

			sampler2D _TwistTexture;
			sampler2D _TextureSample0;


			float3 RotateAroundAxis( float3 center, float3 original, float3 u, float angle )
			{
				original -= center;
				float C = cos( angle );
				float S = sin( angle );
				float t = 1 - C;
				float m00 = t * u.x * u.x + C;
				float m01 = t * u.x * u.y - S * u.z;
				float m02 = t * u.x * u.z + S * u.y;
				float m10 = t * u.x * u.y + S * u.z;
				float m11 = t * u.y * u.y + C;
				float m12 = t * u.y * u.z - S * u.x;
				float m20 = t * u.x * u.z - S * u.y;
				float m21 = t * u.y * u.z + S * u.x;
				float m22 = t * u.z * u.z + C;
				float3x3 finalMatrix = float3x3( m00, m01, m02, m10, m11, m12, m20, m21, m22 );
				return mul( finalMatrix, original ) + center;
			}
			
			float3 mod2D289( float3 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float2 mod2D289( float2 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float3 permute( float3 x ) { return mod2D289( ( ( x * 34.0 ) + 1.0 ) * x ); }
			float snoise( float2 v )
			{
				const float4 C = float4( 0.211324865405187, 0.366025403784439, -0.577350269189626, 0.024390243902439 );
				float2 i = floor( v + dot( v, C.yy ) );
				float2 x0 = v - i + dot( i, C.xx );
				float2 i1;
				i1 = ( x0.x > x0.y ) ? float2( 1.0, 0.0 ) : float2( 0.0, 1.0 );
				float4 x12 = x0.xyxy + C.xxzz;
				x12.xy -= i1;
				i = mod2D289( i );
				float3 p = permute( permute( i.y + float3( 0.0, i1.y, 1.0 ) ) + i.x + float3( 0.0, i1.x, 1.0 ) );
				float3 m = max( 0.5 - float3( dot( x0, x0 ), dot( x12.xy, x12.xy ), dot( x12.zw, x12.zw ) ), 0.0 );
				m = m * m;
				m = m * m;
				float3 x = 2.0 * frac( p * C.www ) - 1.0;
				float3 h = abs( x ) - 0.5;
				float3 ox = floor( x + 0.5 );
				float3 a0 = x - ox;
				m *= 1.79284291400159 - 0.85373472095314 * ( a0 * a0 + h * h );
				float3 g;
				g.x = a0.x * x0.x + h.x * x0.y;
				g.yz = a0.yz * x12.xz + h.yz * x12.yw;
				return 130.0 * dot( m, g );
			}
			

			int _ObjectId;
			int _PassValue;

			struct SurfaceDescription
			{
				float Alpha;
				float AlphaClipThreshold;
			};

			VertexOutput VertexFunction(VertexInput v  )
			{
				VertexOutput o;
				ZERO_INITIALIZE(VertexOutput, o);

				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				float4 transform69 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_0 = (transform69.y).xx;
				float dotResult4_g1 = dot( temp_cast_0 , float2( 12.9898,78.233 ) );
				float lerpResult10_g1 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g1 ) * 43758.55 ) ));
				float temp_output_70_0 = lerpResult10_g1;
				float2 temp_cast_1 = (temp_output_70_0).xx;
				float dotResult4_g2 = dot( temp_cast_1 , float2( 12.9898,78.233 ) );
				float lerpResult10_g2 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g2 ) * 43758.55 ) ));
				float temp_output_93_0 = lerpResult10_g2;
				float RandomViaY0296 = temp_output_93_0;
				float4 transform64 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float TornadoHeight60 = ( transform64.y / ( 2.0 * PI ) );
				float mulTime34 = _TimeParameters.x * _LocalRotationSpeed;
				float2 temp_cast_2 = (temp_output_93_0).xx;
				float dotResult4_g3 = dot( temp_cast_2 , float2( 12.9898,78.233 ) );
				float lerpResult10_g3 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g3 ) * 43758.55 ) ));
				float temp_output_106_0 = lerpResult10_g3;
				float2 temp_cast_3 = (temp_output_106_0).xx;
				float dotResult4_g4 = dot( temp_cast_3 , float2( 12.9898,78.233 ) );
				float lerpResult10_g4 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g4 ) * 43758.55 ) ));
				float RandomViaY03149 = lerpResult10_g4;
				float3 rotatedValue31 = RotateAroundAxis( float3( 0,0,0 ), v.positionOS.xyz, normalize( _LocalRotationAxis ), ( mulTime34 + ( RandomViaY03149 * 100.0 ) ) );
				float3 rotatedValue22 = RotateAroundAxis( float3( 0,0,0 ), rotatedValue31, float3( 1,0,0 ), ( _BendStrength * sign( rotatedValue31.z ) * abs( rotatedValue31.z ) ) );
				float3 objToWorld16 = mul( GetObjectToWorldMatrix(), float4( rotatedValue22, 1 ) ).xyz;
				float3 LocallyBendAndRotatedToWorld140 = objToWorld16;
				float temp_output_144_0 = ( LocallyBendAndRotatedToWorld140.y / ( 2.0 * PI ) );
				float lerpResult146 = lerp( temp_output_144_0 , TornadoHeight60 , _WrapAroundTwistedHose);
				float mulTime41 = _TimeParameters.x * _TwistScrollX;
				float2 temp_cast_4 = (( ( ( lerpResult146 - mulTime41 ) * _TwistScale ) + ( _TwistOffsetLag * ( 1.0 + RandomViaY03149 ) ) )).xx;
				float temp_output_54_0 = sin( ( ( _TwistTop * lerpResult146 ) * PI ) );
				float2 temp_cast_5 = (( ( ( lerpResult146 - mulTime41 ) * _TwistScale ) + ( _TwistOffsetLag * ( 1.0 + RandomViaY03149 ) ) )).xx;
				float3 appendResult58 = (float3(( sin( ( ( tex2Dlod( _TwistTexture, float4( temp_cast_4, 0, 0.0) ).r * 2.0 ) * PI ) ) * _TwistStrength * temp_output_54_0 ) , 0.0 , ( cos( ( ( tex2Dlod( _TwistTexture, float4( temp_cast_4, 0, 0.0) ).r * 2.0 ) * PI ) ) * _TwistStrength * temp_output_54_0 )));
				float3 TwistOffset59 = appendResult58;
				float lerpResult122 = lerp( _RadiusBottom , _RadiusTop , pow( temp_output_144_0 , _RadiusLerpExpo ));
				float3 appendResult127 = (float3(lerpResult122 , 0.0 , 0.0));
				float3 break26 = objToWorld16;
				float3 appendResult15 = (float3(( break26.x + ( _OffsetFromHose + ( _OffsetFromHose * _OffsetFromHoseNoise * temp_output_106_0 ) ) ) , break26.y , break26.z));
				float3 rotatedValue18 = RotateAroundAxis( TwistOffset59, ( TwistOffset59 + ( appendResult127 + appendResult15 ) ), float3( 0,1,0 ), ( ( _TimeParameters.x * ( ( ( -_RotationSpeedNoise * RandomViaY0296 ) + 1.0 ) * _RotationSpeed ) * ( 1.0 + ( -_RotateSlowerOnTop * saturate( TornadoHeight60 ) ) ) ) + ( ( 2.0 * PI ) * RandomViaY0296 ) ) );
				float2 temp_cast_6 = (( ( ( _TimeParameters.x * _WaveTexScrollSpeed ) + ( temp_output_70_0 * 100.0 ) ) * _WaveTexScale )).xx;
				float simplePerlin2D108 = snoise( temp_cast_6*_WaveTexScale );
				simplePerlin2D108 = simplePerlin2D108*0.5 + 0.5;
				float3 appendResult86 = (float3(0.0 , ( _WaveStrengthY * simplePerlin2D108 ) , 0.0));
				float3 WaveOffset85 = appendResult86;
				float3 worldToObj17 = mul( GetWorldToObjectMatrix(), float4( ( rotatedValue18 + WaveOffset85 ), 1 ) ).xyz;
				
				o.ase_texcoord.xy = v.ase_texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord.zw = 0;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.positionOS.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif

				float3 vertexValue = worldToObj17;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.positionOS.xyz = vertexValue;
				#else
					v.positionOS.xyz += vertexValue;
				#endif

				v.normalOS = v.normalOS;

				float3 positionWS = TransformObjectToWorld( v.positionOS.xyz );

				o.positionCS = TransformWorldToHClip(positionWS);

				return o;
			}

			#if defined(ASE_TESSELLATION)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 normalOS : NORMAL;
				float4 ase_texcoord : TEXCOORD0;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.positionOS;
				o.normalOS = v.normalOS;
				o.ase_texcoord = v.ase_texcoord;
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
				return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.positionOS = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.normalOS = patch[0].normalOS * bary.x + patch[1].normalOS * bary.y + patch[2].normalOS * bary.z;
				o.ase_texcoord = patch[0].ase_texcoord * bary.x + patch[1].ase_texcoord * bary.y + patch[2].ase_texcoord * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.positionOS.xyz - patch[i].normalOS * (dot(o.positionOS.xyz, patch[i].normalOS) - dot(patch[i].vertex.xyz, patch[i].normalOS));
				float phongStrength = _TessPhongStrength;
				o.positionOS.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.positionOS.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			half4 frag(VertexOutput IN ) : SV_TARGET
			{
				SurfaceDescription surfaceDescription = (SurfaceDescription)0;

				float2 uv_TextureSample0 = IN.ase_texcoord.xy * _TextureSample0_ST.xy + _TextureSample0_ST.zw;
				float4 tex2DNode10 = tex2D( _TextureSample0, uv_TextureSample0 );
				float lerpResult100 = lerp( tex2DNode10.a , 1.0 , _TexVsColor);
				

				surfaceDescription.Alpha = lerpResult100;
				surfaceDescription.AlphaClipThreshold = 0.1;

				#if _ALPHATEST_ON
					float alphaClipThreshold = 0.01f;
					#if ALPHA_CLIP_THRESHOLD
						alphaClipThreshold = surfaceDescription.AlphaClipThreshold;
					#endif
					clip(surfaceDescription.Alpha - alphaClipThreshold);
				#endif

				half4 outColor = half4(_ObjectId, _PassValue, 1.0, 1.0);
				return outColor;
			}
			ENDHLSL
		}

		
		Pass
		{
			
			Name "ScenePickingPass"
			Tags { "LightMode"="Picking" }

			AlphaToMask Off

			HLSLPROGRAM

			

			#define ASE_FOG 1
			#define ASE_ABSOLUTE_VERTEX_POS 1
			#define _ALPHATEST_ON 1
			#define ASE_SRP_VERSION 140010


			

			#pragma vertex vert
			#pragma fragment frag

			#define ATTRIBUTES_NEED_NORMAL
			#define ATTRIBUTES_NEED_TANGENT

			#define SHADERPASS SHADERPASS_DEPTHONLY

			
            #if ASE_SRP_VERSION >=140007
			#include_with_pragmas "Packages/com.unity.render-pipelines.universal/ShaderLibrary/DOTS.hlsl"
			#endif
		

			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Texture.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/TextureStack.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/Editor/ShaderGraph/Includes/ShaderPass.hlsl"

			#if defined(LOD_FADE_CROSSFADE)
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/LODCrossFade.hlsl"
            #endif

			#define ASE_NEEDS_VERT_POSITION


			struct VertexInput
			{
				float4 positionOS : POSITION;
				float3 normalOS : NORMAL;
				float4 ase_texcoord : TEXCOORD0;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 positionCS : SV_POSITION;
				float4 ase_texcoord : TEXCOORD0;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _Color1;
			float4 _TextureSample0_ST;
			float4 _Color0;
			float3 _LocalRotationAxis;
			float _RotationSpeedNoise;
			float _ColorGradientAddToVertexY;
			float _ColorGradient;
			float _ValueMulti;
			float _SaturationMulti;
			float _HueShift;
			float _WaveTexScale;
			float _WaveTexScrollSpeed;
			float _WaveStrengthY;
			float _OffsetFromHoseNoise;
			float _OffsetFromHose;
			float _RadiusLerpExpo;
			float _RadiusBottom;
			float _TwistTop;
			float _TwistStrength;
			float _TwistOffsetLag;
			float _TwistScale;
			float _TwistScrollX;
			float _WrapAroundTwistedHose;
			float _LocalRotationSpeed;
			float _BendStrength;
			float _RotateSlowerOnTop;
			float _RotationSpeed;
			float _RadiusTop;
			float _TexVsColor;
			#ifdef ASE_TESSELLATION
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END

			sampler2D _TwistTexture;
			sampler2D _TextureSample0;


			float3 RotateAroundAxis( float3 center, float3 original, float3 u, float angle )
			{
				original -= center;
				float C = cos( angle );
				float S = sin( angle );
				float t = 1 - C;
				float m00 = t * u.x * u.x + C;
				float m01 = t * u.x * u.y - S * u.z;
				float m02 = t * u.x * u.z + S * u.y;
				float m10 = t * u.x * u.y + S * u.z;
				float m11 = t * u.y * u.y + C;
				float m12 = t * u.y * u.z - S * u.x;
				float m20 = t * u.x * u.z - S * u.y;
				float m21 = t * u.y * u.z + S * u.x;
				float m22 = t * u.z * u.z + C;
				float3x3 finalMatrix = float3x3( m00, m01, m02, m10, m11, m12, m20, m21, m22 );
				return mul( finalMatrix, original ) + center;
			}
			
			float3 mod2D289( float3 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float2 mod2D289( float2 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float3 permute( float3 x ) { return mod2D289( ( ( x * 34.0 ) + 1.0 ) * x ); }
			float snoise( float2 v )
			{
				const float4 C = float4( 0.211324865405187, 0.366025403784439, -0.577350269189626, 0.024390243902439 );
				float2 i = floor( v + dot( v, C.yy ) );
				float2 x0 = v - i + dot( i, C.xx );
				float2 i1;
				i1 = ( x0.x > x0.y ) ? float2( 1.0, 0.0 ) : float2( 0.0, 1.0 );
				float4 x12 = x0.xyxy + C.xxzz;
				x12.xy -= i1;
				i = mod2D289( i );
				float3 p = permute( permute( i.y + float3( 0.0, i1.y, 1.0 ) ) + i.x + float3( 0.0, i1.x, 1.0 ) );
				float3 m = max( 0.5 - float3( dot( x0, x0 ), dot( x12.xy, x12.xy ), dot( x12.zw, x12.zw ) ), 0.0 );
				m = m * m;
				m = m * m;
				float3 x = 2.0 * frac( p * C.www ) - 1.0;
				float3 h = abs( x ) - 0.5;
				float3 ox = floor( x + 0.5 );
				float3 a0 = x - ox;
				m *= 1.79284291400159 - 0.85373472095314 * ( a0 * a0 + h * h );
				float3 g;
				g.x = a0.x * x0.x + h.x * x0.y;
				g.yz = a0.yz * x12.xz + h.yz * x12.yw;
				return 130.0 * dot( m, g );
			}
			

			float4 _SelectionID;

			struct SurfaceDescription
			{
				float Alpha;
				float AlphaClipThreshold;
			};

			VertexOutput VertexFunction(VertexInput v  )
			{
				VertexOutput o;
				ZERO_INITIALIZE(VertexOutput, o);

				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				float4 transform69 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_0 = (transform69.y).xx;
				float dotResult4_g1 = dot( temp_cast_0 , float2( 12.9898,78.233 ) );
				float lerpResult10_g1 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g1 ) * 43758.55 ) ));
				float temp_output_70_0 = lerpResult10_g1;
				float2 temp_cast_1 = (temp_output_70_0).xx;
				float dotResult4_g2 = dot( temp_cast_1 , float2( 12.9898,78.233 ) );
				float lerpResult10_g2 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g2 ) * 43758.55 ) ));
				float temp_output_93_0 = lerpResult10_g2;
				float RandomViaY0296 = temp_output_93_0;
				float4 transform64 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float TornadoHeight60 = ( transform64.y / ( 2.0 * PI ) );
				float mulTime34 = _TimeParameters.x * _LocalRotationSpeed;
				float2 temp_cast_2 = (temp_output_93_0).xx;
				float dotResult4_g3 = dot( temp_cast_2 , float2( 12.9898,78.233 ) );
				float lerpResult10_g3 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g3 ) * 43758.55 ) ));
				float temp_output_106_0 = lerpResult10_g3;
				float2 temp_cast_3 = (temp_output_106_0).xx;
				float dotResult4_g4 = dot( temp_cast_3 , float2( 12.9898,78.233 ) );
				float lerpResult10_g4 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g4 ) * 43758.55 ) ));
				float RandomViaY03149 = lerpResult10_g4;
				float3 rotatedValue31 = RotateAroundAxis( float3( 0,0,0 ), v.positionOS.xyz, normalize( _LocalRotationAxis ), ( mulTime34 + ( RandomViaY03149 * 100.0 ) ) );
				float3 rotatedValue22 = RotateAroundAxis( float3( 0,0,0 ), rotatedValue31, float3( 1,0,0 ), ( _BendStrength * sign( rotatedValue31.z ) * abs( rotatedValue31.z ) ) );
				float3 objToWorld16 = mul( GetObjectToWorldMatrix(), float4( rotatedValue22, 1 ) ).xyz;
				float3 LocallyBendAndRotatedToWorld140 = objToWorld16;
				float temp_output_144_0 = ( LocallyBendAndRotatedToWorld140.y / ( 2.0 * PI ) );
				float lerpResult146 = lerp( temp_output_144_0 , TornadoHeight60 , _WrapAroundTwistedHose);
				float mulTime41 = _TimeParameters.x * _TwistScrollX;
				float2 temp_cast_4 = (( ( ( lerpResult146 - mulTime41 ) * _TwistScale ) + ( _TwistOffsetLag * ( 1.0 + RandomViaY03149 ) ) )).xx;
				float temp_output_54_0 = sin( ( ( _TwistTop * lerpResult146 ) * PI ) );
				float2 temp_cast_5 = (( ( ( lerpResult146 - mulTime41 ) * _TwistScale ) + ( _TwistOffsetLag * ( 1.0 + RandomViaY03149 ) ) )).xx;
				float3 appendResult58 = (float3(( sin( ( ( tex2Dlod( _TwistTexture, float4( temp_cast_4, 0, 0.0) ).r * 2.0 ) * PI ) ) * _TwistStrength * temp_output_54_0 ) , 0.0 , ( cos( ( ( tex2Dlod( _TwistTexture, float4( temp_cast_4, 0, 0.0) ).r * 2.0 ) * PI ) ) * _TwistStrength * temp_output_54_0 )));
				float3 TwistOffset59 = appendResult58;
				float lerpResult122 = lerp( _RadiusBottom , _RadiusTop , pow( temp_output_144_0 , _RadiusLerpExpo ));
				float3 appendResult127 = (float3(lerpResult122 , 0.0 , 0.0));
				float3 break26 = objToWorld16;
				float3 appendResult15 = (float3(( break26.x + ( _OffsetFromHose + ( _OffsetFromHose * _OffsetFromHoseNoise * temp_output_106_0 ) ) ) , break26.y , break26.z));
				float3 rotatedValue18 = RotateAroundAxis( TwistOffset59, ( TwistOffset59 + ( appendResult127 + appendResult15 ) ), float3( 0,1,0 ), ( ( _TimeParameters.x * ( ( ( -_RotationSpeedNoise * RandomViaY0296 ) + 1.0 ) * _RotationSpeed ) * ( 1.0 + ( -_RotateSlowerOnTop * saturate( TornadoHeight60 ) ) ) ) + ( ( 2.0 * PI ) * RandomViaY0296 ) ) );
				float2 temp_cast_6 = (( ( ( _TimeParameters.x * _WaveTexScrollSpeed ) + ( temp_output_70_0 * 100.0 ) ) * _WaveTexScale )).xx;
				float simplePerlin2D108 = snoise( temp_cast_6*_WaveTexScale );
				simplePerlin2D108 = simplePerlin2D108*0.5 + 0.5;
				float3 appendResult86 = (float3(0.0 , ( _WaveStrengthY * simplePerlin2D108 ) , 0.0));
				float3 WaveOffset85 = appendResult86;
				float3 worldToObj17 = mul( GetWorldToObjectMatrix(), float4( ( rotatedValue18 + WaveOffset85 ), 1 ) ).xyz;
				
				o.ase_texcoord.xy = v.ase_texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord.zw = 0;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.positionOS.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif

				float3 vertexValue = worldToObj17;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.positionOS.xyz = vertexValue;
				#else
					v.positionOS.xyz += vertexValue;
				#endif

				v.normalOS = v.normalOS;

				float3 positionWS = TransformObjectToWorld( v.positionOS.xyz );
				o.positionCS = TransformWorldToHClip(positionWS);
				return o;
			}

			#if defined(ASE_TESSELLATION)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 normalOS : NORMAL;
				float4 ase_texcoord : TEXCOORD0;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.positionOS;
				o.normalOS = v.normalOS;
				o.ase_texcoord = v.ase_texcoord;
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
				return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.positionOS = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.normalOS = patch[0].normalOS * bary.x + patch[1].normalOS * bary.y + patch[2].normalOS * bary.z;
				o.ase_texcoord = patch[0].ase_texcoord * bary.x + patch[1].ase_texcoord * bary.y + patch[2].ase_texcoord * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.positionOS.xyz - patch[i].normalOS * (dot(o.positionOS.xyz, patch[i].normalOS) - dot(patch[i].vertex.xyz, patch[i].normalOS));
				float phongStrength = _TessPhongStrength;
				o.positionOS.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.positionOS.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			half4 frag(VertexOutput IN ) : SV_TARGET
			{
				SurfaceDescription surfaceDescription = (SurfaceDescription)0;

				float2 uv_TextureSample0 = IN.ase_texcoord.xy * _TextureSample0_ST.xy + _TextureSample0_ST.zw;
				float4 tex2DNode10 = tex2D( _TextureSample0, uv_TextureSample0 );
				float lerpResult100 = lerp( tex2DNode10.a , 1.0 , _TexVsColor);
				

				surfaceDescription.Alpha = lerpResult100;
				surfaceDescription.AlphaClipThreshold = 0.1;

				#if _ALPHATEST_ON
					float alphaClipThreshold = 0.01f;
					#if ALPHA_CLIP_THRESHOLD
						alphaClipThreshold = surfaceDescription.AlphaClipThreshold;
					#endif
					clip(surfaceDescription.Alpha - alphaClipThreshold);
				#endif

				half4 outColor = 0;
				outColor = _SelectionID;

				return outColor;
			}

			ENDHLSL
		}

		
		Pass
		{
			
			Name "DepthNormals"
			Tags { "LightMode"="DepthNormalsOnly" }

			ZTest LEqual
			ZWrite On

			HLSLPROGRAM

			

			#pragma multi_compile_instancing
			#pragma multi_compile _ LOD_FADE_CROSSFADE
			#define ASE_FOG 1
			#define ASE_ABSOLUTE_VERTEX_POS 1
			#define _ALPHATEST_ON 1
			#define ASE_SRP_VERSION 140010


			

			#pragma vertex vert
			#pragma fragment frag

        	#pragma multi_compile_fragment _ _GBUFFER_NORMALS_OCT

			

			#define ATTRIBUTES_NEED_NORMAL
			#define ATTRIBUTES_NEED_TANGENT
			#define VARYINGS_NEED_NORMAL_WS

			#define SHADERPASS SHADERPASS_DEPTHNORMALSONLY

			
            #if ASE_SRP_VERSION >=140007
			#include_with_pragmas "Packages/com.unity.render-pipelines.universal/ShaderLibrary/DOTS.hlsl"
			#endif
		

			
			#if ASE_SRP_VERSION >=140007
			#include_with_pragmas "Packages/com.unity.render-pipelines.universal/ShaderLibrary/RenderingLayers.hlsl"
			#endif
		

			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Texture.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/TextureStack.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/Editor/ShaderGraph/Includes/ShaderPass.hlsl"

            #if defined(LOD_FADE_CROSSFADE)
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/LODCrossFade.hlsl"
            #endif

			#define ASE_NEEDS_VERT_POSITION


			struct VertexInput
			{
				float4 positionOS : POSITION;
				float3 normalOS : NORMAL;
				float4 ase_texcoord : TEXCOORD0;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 positionCS : SV_POSITION;
				float3 normalWS : TEXCOORD0;
				float4 ase_texcoord1 : TEXCOORD1;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _Color1;
			float4 _TextureSample0_ST;
			float4 _Color0;
			float3 _LocalRotationAxis;
			float _RotationSpeedNoise;
			float _ColorGradientAddToVertexY;
			float _ColorGradient;
			float _ValueMulti;
			float _SaturationMulti;
			float _HueShift;
			float _WaveTexScale;
			float _WaveTexScrollSpeed;
			float _WaveStrengthY;
			float _OffsetFromHoseNoise;
			float _OffsetFromHose;
			float _RadiusLerpExpo;
			float _RadiusBottom;
			float _TwistTop;
			float _TwistStrength;
			float _TwistOffsetLag;
			float _TwistScale;
			float _TwistScrollX;
			float _WrapAroundTwistedHose;
			float _LocalRotationSpeed;
			float _BendStrength;
			float _RotateSlowerOnTop;
			float _RotationSpeed;
			float _RadiusTop;
			float _TexVsColor;
			#ifdef ASE_TESSELLATION
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END

			sampler2D _TwistTexture;
			sampler2D _TextureSample0;


			float3 RotateAroundAxis( float3 center, float3 original, float3 u, float angle )
			{
				original -= center;
				float C = cos( angle );
				float S = sin( angle );
				float t = 1 - C;
				float m00 = t * u.x * u.x + C;
				float m01 = t * u.x * u.y - S * u.z;
				float m02 = t * u.x * u.z + S * u.y;
				float m10 = t * u.x * u.y + S * u.z;
				float m11 = t * u.y * u.y + C;
				float m12 = t * u.y * u.z - S * u.x;
				float m20 = t * u.x * u.z - S * u.y;
				float m21 = t * u.y * u.z + S * u.x;
				float m22 = t * u.z * u.z + C;
				float3x3 finalMatrix = float3x3( m00, m01, m02, m10, m11, m12, m20, m21, m22 );
				return mul( finalMatrix, original ) + center;
			}
			
			float3 mod2D289( float3 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float2 mod2D289( float2 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float3 permute( float3 x ) { return mod2D289( ( ( x * 34.0 ) + 1.0 ) * x ); }
			float snoise( float2 v )
			{
				const float4 C = float4( 0.211324865405187, 0.366025403784439, -0.577350269189626, 0.024390243902439 );
				float2 i = floor( v + dot( v, C.yy ) );
				float2 x0 = v - i + dot( i, C.xx );
				float2 i1;
				i1 = ( x0.x > x0.y ) ? float2( 1.0, 0.0 ) : float2( 0.0, 1.0 );
				float4 x12 = x0.xyxy + C.xxzz;
				x12.xy -= i1;
				i = mod2D289( i );
				float3 p = permute( permute( i.y + float3( 0.0, i1.y, 1.0 ) ) + i.x + float3( 0.0, i1.x, 1.0 ) );
				float3 m = max( 0.5 - float3( dot( x0, x0 ), dot( x12.xy, x12.xy ), dot( x12.zw, x12.zw ) ), 0.0 );
				m = m * m;
				m = m * m;
				float3 x = 2.0 * frac( p * C.www ) - 1.0;
				float3 h = abs( x ) - 0.5;
				float3 ox = floor( x + 0.5 );
				float3 a0 = x - ox;
				m *= 1.79284291400159 - 0.85373472095314 * ( a0 * a0 + h * h );
				float3 g;
				g.x = a0.x * x0.x + h.x * x0.y;
				g.yz = a0.yz * x12.xz + h.yz * x12.yw;
				return 130.0 * dot( m, g );
			}
			

			struct SurfaceDescription
			{
				float Alpha;
				float AlphaClipThreshold;
			};

			VertexOutput VertexFunction(VertexInput v  )
			{
				VertexOutput o;
				ZERO_INITIALIZE(VertexOutput, o);

				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				float4 transform69 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 temp_cast_0 = (transform69.y).xx;
				float dotResult4_g1 = dot( temp_cast_0 , float2( 12.9898,78.233 ) );
				float lerpResult10_g1 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g1 ) * 43758.55 ) ));
				float temp_output_70_0 = lerpResult10_g1;
				float2 temp_cast_1 = (temp_output_70_0).xx;
				float dotResult4_g2 = dot( temp_cast_1 , float2( 12.9898,78.233 ) );
				float lerpResult10_g2 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g2 ) * 43758.55 ) ));
				float temp_output_93_0 = lerpResult10_g2;
				float RandomViaY0296 = temp_output_93_0;
				float4 transform64 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float TornadoHeight60 = ( transform64.y / ( 2.0 * PI ) );
				float mulTime34 = _TimeParameters.x * _LocalRotationSpeed;
				float2 temp_cast_2 = (temp_output_93_0).xx;
				float dotResult4_g3 = dot( temp_cast_2 , float2( 12.9898,78.233 ) );
				float lerpResult10_g3 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g3 ) * 43758.55 ) ));
				float temp_output_106_0 = lerpResult10_g3;
				float2 temp_cast_3 = (temp_output_106_0).xx;
				float dotResult4_g4 = dot( temp_cast_3 , float2( 12.9898,78.233 ) );
				float lerpResult10_g4 = lerp( 0.0 , 1.0 , frac( ( sin( dotResult4_g4 ) * 43758.55 ) ));
				float RandomViaY03149 = lerpResult10_g4;
				float3 rotatedValue31 = RotateAroundAxis( float3( 0,0,0 ), v.positionOS.xyz, normalize( _LocalRotationAxis ), ( mulTime34 + ( RandomViaY03149 * 100.0 ) ) );
				float3 rotatedValue22 = RotateAroundAxis( float3( 0,0,0 ), rotatedValue31, float3( 1,0,0 ), ( _BendStrength * sign( rotatedValue31.z ) * abs( rotatedValue31.z ) ) );
				float3 objToWorld16 = mul( GetObjectToWorldMatrix(), float4( rotatedValue22, 1 ) ).xyz;
				float3 LocallyBendAndRotatedToWorld140 = objToWorld16;
				float temp_output_144_0 = ( LocallyBendAndRotatedToWorld140.y / ( 2.0 * PI ) );
				float lerpResult146 = lerp( temp_output_144_0 , TornadoHeight60 , _WrapAroundTwistedHose);
				float mulTime41 = _TimeParameters.x * _TwistScrollX;
				float2 temp_cast_4 = (( ( ( lerpResult146 - mulTime41 ) * _TwistScale ) + ( _TwistOffsetLag * ( 1.0 + RandomViaY03149 ) ) )).xx;
				float temp_output_54_0 = sin( ( ( _TwistTop * lerpResult146 ) * PI ) );
				float2 temp_cast_5 = (( ( ( lerpResult146 - mulTime41 ) * _TwistScale ) + ( _TwistOffsetLag * ( 1.0 + RandomViaY03149 ) ) )).xx;
				float3 appendResult58 = (float3(( sin( ( ( tex2Dlod( _TwistTexture, float4( temp_cast_4, 0, 0.0) ).r * 2.0 ) * PI ) ) * _TwistStrength * temp_output_54_0 ) , 0.0 , ( cos( ( ( tex2Dlod( _TwistTexture, float4( temp_cast_4, 0, 0.0) ).r * 2.0 ) * PI ) ) * _TwistStrength * temp_output_54_0 )));
				float3 TwistOffset59 = appendResult58;
				float lerpResult122 = lerp( _RadiusBottom , _RadiusTop , pow( temp_output_144_0 , _RadiusLerpExpo ));
				float3 appendResult127 = (float3(lerpResult122 , 0.0 , 0.0));
				float3 break26 = objToWorld16;
				float3 appendResult15 = (float3(( break26.x + ( _OffsetFromHose + ( _OffsetFromHose * _OffsetFromHoseNoise * temp_output_106_0 ) ) ) , break26.y , break26.z));
				float3 rotatedValue18 = RotateAroundAxis( TwistOffset59, ( TwistOffset59 + ( appendResult127 + appendResult15 ) ), float3( 0,1,0 ), ( ( _TimeParameters.x * ( ( ( -_RotationSpeedNoise * RandomViaY0296 ) + 1.0 ) * _RotationSpeed ) * ( 1.0 + ( -_RotateSlowerOnTop * saturate( TornadoHeight60 ) ) ) ) + ( ( 2.0 * PI ) * RandomViaY0296 ) ) );
				float2 temp_cast_6 = (( ( ( _TimeParameters.x * _WaveTexScrollSpeed ) + ( temp_output_70_0 * 100.0 ) ) * _WaveTexScale )).xx;
				float simplePerlin2D108 = snoise( temp_cast_6*_WaveTexScale );
				simplePerlin2D108 = simplePerlin2D108*0.5 + 0.5;
				float3 appendResult86 = (float3(0.0 , ( _WaveStrengthY * simplePerlin2D108 ) , 0.0));
				float3 WaveOffset85 = appendResult86;
				float3 worldToObj17 = mul( GetWorldToObjectMatrix(), float4( ( rotatedValue18 + WaveOffset85 ), 1 ) ).xyz;
				
				o.ase_texcoord1.xy = v.ase_texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord1.zw = 0;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.positionOS.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif

				float3 vertexValue = worldToObj17;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.positionOS.xyz = vertexValue;
				#else
					v.positionOS.xyz += vertexValue;
				#endif

				v.normalOS = v.normalOS;

				float3 positionWS = TransformObjectToWorld( v.positionOS.xyz );
				float3 normalWS = TransformObjectToWorldNormal(v.normalOS);

				o.positionCS = TransformWorldToHClip(positionWS);
				o.normalWS.xyz =  normalWS;

				return o;
			}

			#if defined(ASE_TESSELLATION)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 normalOS : NORMAL;
				float4 ase_texcoord : TEXCOORD0;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.positionOS;
				o.normalOS = v.normalOS;
				o.ase_texcoord = v.ase_texcoord;
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
				return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.positionOS = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.normalOS = patch[0].normalOS * bary.x + patch[1].normalOS * bary.y + patch[2].normalOS * bary.z;
				o.ase_texcoord = patch[0].ase_texcoord * bary.x + patch[1].ase_texcoord * bary.y + patch[2].ase_texcoord * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.positionOS.xyz - patch[i].normalOS * (dot(o.positionOS.xyz, patch[i].normalOS) - dot(patch[i].vertex.xyz, patch[i].normalOS));
				float phongStrength = _TessPhongStrength;
				o.positionOS.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.positionOS.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			void frag( VertexOutput IN
				, out half4 outNormalWS : SV_Target0
			#ifdef _WRITE_RENDERING_LAYERS
				, out float4 outRenderingLayers : SV_Target1
			#endif
				 )
			{
				SurfaceDescription surfaceDescription = (SurfaceDescription)0;

				float2 uv_TextureSample0 = IN.ase_texcoord1.xy * _TextureSample0_ST.xy + _TextureSample0_ST.zw;
				float4 tex2DNode10 = tex2D( _TextureSample0, uv_TextureSample0 );
				float lerpResult100 = lerp( tex2DNode10.a , 1.0 , _TexVsColor);
				

				surfaceDescription.Alpha = lerpResult100;
				surfaceDescription.AlphaClipThreshold = 0.1;

				#if _ALPHATEST_ON
					clip(surfaceDescription.Alpha - surfaceDescription.AlphaClipThreshold);
				#endif

				#ifdef LOD_FADE_CROSSFADE
					LODFadeCrossFade( IN.positionCS );
				#endif

				#if defined(_GBUFFER_NORMALS_OCT)
					float3 normalWS = normalize(IN.normalWS);
					float2 octNormalWS = PackNormalOctQuadEncode(normalWS);           // values between [-1, +1], must use fp32 on some platforms
					float2 remappedOctNormalWS = saturate(octNormalWS * 0.5 + 0.5);   // values between [ 0,  1]
					half3 packedNormalWS = PackFloat2To888(remappedOctNormalWS);      // values between [ 0,  1]
					outNormalWS = half4(packedNormalWS, 0.0);
				#else
					float3 normalWS = IN.normalWS;
					outNormalWS = half4(NormalizeNormalPerPixel(normalWS), 0.0);
				#endif

				#ifdef _WRITE_RENDERING_LAYERS
					uint renderingLayers = GetMeshRenderingLayer();
					outRenderingLayers = float4(EncodeMeshRenderingLayer(renderingLayers), 0, 0, 0);
				#endif
			}

			ENDHLSL
		}

	
	}
	
	CustomEditor "UnityEditor.ShaderGraphUnlitGUI"
	FallBack "Hidden/Shader Graph/FallbackError"
	
	Fallback Off
}
/*ASEBEGIN
Version=19303
Node;AmplifyShaderEditor.CommentaryNode;194;-3883.717,192;Inherit;False;1311.717;707.3378;;7;69;93;70;97;96;150;106;Random;0.335849,0.07223921,0.3267491,1;0;0
Node;AmplifyShaderEditor.ObjectToWorldTransfNode;69;-3808,240;Inherit;False;1;0;FLOAT4;0,0,0,1;False;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.FunctionNode;70;-3536,288;Inherit;False;Random Range;-1;;1;7b754edb8aebbfb4a9ace907af661cfc;0;3;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;93;-3328,448;Inherit;False;Random Range;-1;;2;7b754edb8aebbfb4a9ace907af661cfc;0;3;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;106;-3168,720;Inherit;False;Random Range;-1;;3;7b754edb8aebbfb4a9ace907af661cfc;0;3;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;150;-2960,544;Inherit;False;Random Range;-1;;4;7b754edb8aebbfb4a9ace907af661cfc;0;3;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;114;-2752,-448;Inherit;False;822.5769;543.0826;;7;31;34;115;13;116;154;155;Local Rotation;0.1223071,0.3622641,0.120299,1;0;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;149;-2752,608;Inherit;False;RandomViaY03;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;116;-2720,-144;Inherit;False;Property;_LocalRotationSpeed;LocalRotationSpeed;28;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;154;-2560,0;Inherit;False;149;RandomViaY03;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;34;-2528,-80;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;156;-2284.114,52.36554;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;100;False;1;FLOAT;0
Node;AmplifyShaderEditor.PosVertexDataNode;13;-2656,-432;Inherit;False;0;0;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.Vector3Node;115;-2688,-288;Inherit;False;Property;_LocalRotationAxis;LocalRotationAxis;27;0;Create;True;0;0;0;False;0;False;1,1,1;0,0,0;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.SimpleAddOpNode;155;-2304,-112;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;117;-1858,-1130;Inherit;False;884;802.2;;9;25;35;36;37;32;29;27;30;22;Bend;0.2973231,0.4641509,0.0262727,1;0;0
Node;AmplifyShaderEditor.RotateAboutAxisNode;31;-2256,-288;Inherit;False;True;4;0;FLOAT3;0,1,0;False;1;FLOAT;0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.BreakToComponentsNode;32;-1776,-544;Inherit;False;FLOAT3;1;0;FLOAT3;0,0,0;False;16;FLOAT;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT;5;FLOAT;6;FLOAT;7;FLOAT;8;FLOAT;9;FLOAT;10;FLOAT;11;FLOAT;12;FLOAT;13;FLOAT;14;FLOAT;15
Node;AmplifyShaderEditor.RangedFloatNode;25;-1808,-776;Inherit;False;Property;_BendStrength;BendStrength;5;0;Create;True;0;0;0;False;0;False;0;0;-5;5;0;1;FLOAT;0
Node;AmplifyShaderEditor.SignOpNode;29;-1584,-592;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.AbsOpNode;27;-1584,-496;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;30;-1392,-672;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RotateAboutAxisNode;22;-1280,-512;Inherit;False;False;4;0;FLOAT3;1,0,0;False;1;FLOAT;0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.TransformPositionNode;16;-1440,-208;Inherit;False;Object;World;False;Fast;True;1;0;FLOAT3;0,0,0;False;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.CommentaryNode;105;-3360,1632;Inherit;False;788;354.7159;;4;61;64;65;60;Tornado Heigh Relative;1,1,1,1;0;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;140;-1168,112;Inherit;False;LocallyBendAndRotatedToWorld;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;141;-2288,1696;Inherit;False;140;LocallyBendAndRotatedToWorld;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.ObjectToWorldTransfNode;64;-3296,1664;Inherit;False;1;0;FLOAT4;0,0,0,1;False;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.PiNode;61;-3280,1856;Inherit;False;1;0;FLOAT;2;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;38;-1776,2144;Inherit;False;2689.098;834.8;;23;59;58;57;56;55;54;53;52;51;50;49;48;47;46;45;44;43;42;41;39;136;137;148;Twist Offset;0.454902,0.1882353,0.2982451,1;0;0
Node;AmplifyShaderEditor.BreakToComponentsNode;143;-1904,1728;Inherit;False;FLOAT3;1;0;FLOAT3;0,0,0;False;16;FLOAT;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT;5;FLOAT;6;FLOAT;7;FLOAT;8;FLOAT;9;FLOAT;10;FLOAT;11;FLOAT;12;FLOAT;13;FLOAT;14;FLOAT;15
Node;AmplifyShaderEditor.PiNode;145;-2000,1520;Inherit;False;1;0;FLOAT;2;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;65;-3024,1760;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;39;-1776,2688;Inherit;False;Property;_TwistScrollX;TwistScrollX;13;0;Create;True;0;0;0;False;0;False;0;0;0;5;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;144;-1712,1632;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;60;-2832,1824;Inherit;False;TornadoHeight;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;147;-2304,2032;Inherit;False;Property;_WrapAroundTwistedHose;WrapAroundTwistedHose;30;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;41;-1600,2848;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;146;-1824,1952;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;151;-1472,2960;Inherit;False;149;RandomViaY03;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;43;-1696,2512;Inherit;False;Property;_TwistScale;TwistScale;14;0;Create;True;0;0;0;False;0;False;1;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;42;-1408,2704;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;136;-1424,2816;Inherit;False;Property;_TwistOffsetLag;TwistOffsetLag;29;0;Create;True;0;0;0;False;0;False;0;0;-0.1;0.1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;152;-1168,2928;Inherit;False;2;2;0;FLOAT;1;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;44;-1344,2512;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;148;-1088,2768;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;137;-1120,2560;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;46;-1760,2224;Inherit;False;Property;_TwistTop;TwistTop;15;0;Create;True;0;0;0;False;0;False;1;0;0.8;2;0;1;FLOAT;0
Node;AmplifyShaderEditor.SamplerNode;47;-976,2496;Inherit;True;Property;_TwistTexture;TwistTexture;11;0;Create;True;0;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleTimeNode;80;-1936,1040;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;74;-2000,1136;Inherit;False;Property;_WaveTexScrollSpeed;WaveTexScrollSpeed;17;0;Create;True;0;0;0;False;0;False;1;0;0;2;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;48;-688,2480;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;2;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;49;-1376,2320;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;193;-656,-944;Inherit;False;1460;906.2;;23;104;20;134;102;19;21;101;18;63;133;14;195;197;198;199;203;204;205;207;209;210;211;212;RotateAround Twisted Offset;0.3458455,0.421725,0.6150943,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;118;-1152,1424;Inherit;False;1672.533;554.3446;Subtracting one since that the original radius in the vertex positions;7;128;127;122;121;120;119;129;Radius;0.645283,0.1874973,0.1874973,1;0;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;82;-1690.747,1040.193;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;107;-1696,784;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;100;False;1;FLOAT;0
Node;AmplifyShaderEditor.PiNode;50;-512,2480;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.PiNode;51;-1072,2320;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;130;-1136,368;Inherit;False;Property;_OffsetFromHoseNoise;OffsetFromHoseNoise;3;0;Create;True;0;0;0;False;0;False;1;0;0;3;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;12;-1136,240;Inherit;False;Property;_OffsetFromHose;OffsetFromHose;2;0;Create;True;0;0;0;False;0;False;0;0;0;5;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;96;-2864,352;Inherit;False;RandomViaY02;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;133;-320,-912;Inherit;False;Property;_RotationSpeedNoise;RotationSpeedNoise;9;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;81;-1456,880;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SinOpNode;52;-272,2352;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CosOpNode;53;-272,2496;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SinOpNode;54;-848,2320;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;55;-224,2608;Inherit;False;Property;_TwistStrength;TwistStrength;12;0;Create;True;0;0;0;False;0;False;0;0;0;5;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;119;-1104,1776;Inherit;False;Property;_RadiusLerpExpo;RadiusLerpExpo;18;0;Create;True;0;0;0;False;0;False;0.1;0;0;5;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;132;-816,336;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;78;-1664,1184;Inherit;False;Property;_WaveTexScale;WaveTexScale;19;0;Create;True;0;0;0;False;0;False;1;0;0;2;0;1;FLOAT;0
Node;AmplifyShaderEditor.NegateNode;209;-32,-912;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;104;-80,-816;Inherit;False;96;RandomViaY02;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;197;-656,-736;Inherit;False;Property;_RotateSlowerOnTop;RotateSlowerOnTop;33;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;195;-592,-544;Inherit;False;60;TornadoHeight;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;79;-1264,976;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;57;112,2240;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;128;-1072,1472;Inherit;False;Property;_RadiusBottom;RadiusBottom;1;0;Create;True;0;0;0;False;0;False;0;0;0;4;0;1;FLOAT;0
Node;AmplifyShaderEditor.PowerNode;121;-752,1664;Inherit;False;False;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;129;-1072,1568;Inherit;False;Property;_RadiusTop;RadiusTop;4;0;Create;True;0;0;0;False;0;False;2.809793;0;0;4;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;131;-704,208;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;56;208,2464;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.BreakToComponentsNode;26;-992,-96;Inherit;False;FLOAT3;1;0;FLOAT3;0,0,0;False;16;FLOAT;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT;5;FLOAT;6;FLOAT;7;FLOAT;8;FLOAT;9;FLOAT;10;FLOAT;11;FLOAT;12;FLOAT;13;FLOAT;14;FLOAT;15
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;210;160,-912;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.NegateNode;203;-160,-608;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;205;-272,-464;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;20;-544,-896;Inherit;False;Property;_RotationSpeed;RotationSpeed;10;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.NoiseGeneratorNode;108;-1296,672;Inherit;False;Simplex2D;True;False;2;0;FLOAT2;0,0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;67;-576,160;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;72;-704,688;Inherit;False;Property;_WaveStrengthY;WaveStrengthY;16;0;Create;True;0;0;0;False;0;False;0;0;0;5;0;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;122;-512,1536;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;58;400,2272;Inherit;False;FLOAT3;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleAddOpNode;207;304,-816;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;199;48,-528;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;15;-448,32;Inherit;False;FLOAT3;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;84;-366.6855,735.1888;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;127;272,1504;Inherit;False;FLOAT3;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;59;672,2256;Inherit;False;TwistOffset;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleTimeNode;19;448,-896;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;134;416,-704;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.PiNode;102;160,-384;Inherit;False;1;0;FLOAT;2;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;212;454.3226,-545.2288;Inherit;False;2;2;0;FLOAT;1;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;86;-176,800;Inherit;False;FLOAT3;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleAddOpNode;68;-176,48;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;63;160,-304;Inherit;False;59;TwistOffset;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;21;672,-720;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;211;432,-384;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;85;-16,672;Inherit;False;WaveOffset;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleAddOpNode;14;288,-144;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleAddOpNode;101;592,-464;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;111;688,-1904;Inherit;False;1698.496;770.4;;15;89;92;98;99;100;180;181;182;184;185;186;189;190;183;179;FinalColor;0.1634033,0.4312212,0.509434,1;0;0
Node;AmplifyShaderEditor.RotateAboutAxisNode;18;512,-272;Inherit;False;False;4;0;FLOAT3;0,1,0;False;1;FLOAT;0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;88;672,112;Inherit;False;85;WaveOffset;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SamplerNode;10;336,-1776;Inherit;True;Property;_TextureSample0;Texture Sample 0;0;0;Create;True;0;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;89;1072,-1328;Inherit;False;Property;_TexVsColor;TexVsColor;23;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;87;1200,-96;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.TransformPositionNode;157;-1904,-208;Inherit;False;Object;World;False;Fast;True;1;0;FLOAT3;0,0,0;False;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.GetLocalVarNode;120;-1056,1680;Inherit;False;60;TornadoHeight;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;95;-2160,976;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;35;-1808,-888;Inherit;False;Property;_BendNoiseStrength;BendNoiseStrength;6;0;Create;True;0;0;0;False;0;False;0;0;-5;5;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;36;-1808,-984;Inherit;False;Property;_BendNoiseScale;BendNoiseScale;7;0;Create;True;0;0;0;False;0;False;0;0;-5;5;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;37;-1808,-1080;Inherit;False;Property;_BendNoiseSpeed;BendNoiseSpeed;8;0;Create;True;0;0;0;False;0;False;0;0;-5;5;0;1;FLOAT;0
Node;AmplifyShaderEditor.PosVertexDataNode;138;-2224,2384;Inherit;False;0;0;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.TransformPositionNode;139;-2000,2352;Inherit;False;Object;World;False;Fast;True;1;0;FLOAT3;0,0,0;False;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.SimpleSubtractOpNode;142;-1727.104,1784.276;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;45;-1696,2336;Inherit;False;60;TornadoHeight;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;100;1680,-1312;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;1;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;99;2192,-1376;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.LerpOp;92;1888,-1776;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.PosVertexDataNode;189;1872,-1920;Inherit;False;0;0;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;190;2128,-1840;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT;0;False;2;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleAddOpNode;192;2064,-1984;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;188;1840,-2160;Inherit;False;Property;_ColorGradient;ColorGradient;31;0;Create;True;0;0;0;False;0;False;0;0;0;10;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;191;1792,-2080;Inherit;False;Property;_ColorGradientAddToVertexY;ColorGradientAddToVertexY;32;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;178;640,-1472;Inherit;False;Property;_Color2;Color 1;25;1;[HDR];Create;True;0;0;0;False;0;False;0,0,0,0;0,0,0,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.HSVToRGBNode;182;1488,-1776;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.RGBToHSVNode;180;768,-1728;Inherit;False;1;0;FLOAT3;0,0,0;False;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.RangedFloatNode;183;784,-1808;Inherit;False;Property;_SaturationMulti;SaturationMulti;22;0;Create;True;0;0;0;False;0;False;1;0;0;10;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;179;768,-1904;Inherit;False;Property;_HueShift;HueShift;20;0;Create;True;0;0;0;False;0;False;0;0;-1;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;186;1312,-1632;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;185;1264,-1744;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;181;1264,-1856;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;184;944,-1584;Inherit;False;Property;_ValueMulti;ValueMulti;21;0;Create;True;0;0;0;False;0;False;0;0;0;10;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;98;1536,-1520;Inherit;False;97;RandomViaY01g;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;91;1344,-2304;Inherit;False;Property;_Color1;Color 1;26;1;[HDR];Create;True;0;0;0;False;0;False;0,0,0,0;0,0,0,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.ColorNode;90;1328,-2144;Inherit;False;Property;_Color0;Color 0;24;1;[HDR];Create;True;0;0;0;False;0;False;0,0,0,0;0,0,0,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;11;1712,-272;Inherit;False;Constant;_AlphaClipTH;AlphaClipTH;1;0;Create;True;0;0;0;False;0;False;0.1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.TransformPositionNode;17;1744,-112;Inherit;False;World;Object;False;Fast;True;1;0;FLOAT3;0,0,0;False;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.RegisterLocalVarNode;97;-3312,256;Inherit;False;RandomViaY01g;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;198;-624,-640;Inherit;False;Property;_RotateSlowerOnTopExpo;RotateSlowerOnTopExpo;34;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.PowerNode;204;-352,-608;Inherit;False;False;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;168;1472,-320;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;1;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;ExtraPrePass;0;0;ExtraPrePass;5;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;True;1;1;False;;0;False;;0;1;False;;0;False;;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;True;True;True;True;0;False;;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;True;1;False;;True;3;False;;True;True;0;False;;0;False;;True;0;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;170;1472,-320;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;1;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;ShadowCaster;0;2;ShadowCaster;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;False;False;True;False;False;False;False;0;False;;False;False;False;False;False;False;False;False;False;True;1;False;;True;3;False;;False;True;1;LightMode=ShadowCaster;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;171;1472,-320;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;1;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;DepthOnly;0;3;DepthOnly;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;False;False;True;True;False;False;False;0;False;;False;False;False;False;False;False;False;False;False;True;1;False;;False;False;True;1;LightMode=DepthOnly;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;172;1472,-320;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;1;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;Meta;0;4;Meta;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;2;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=Meta;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;173;1472,-320;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;1;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;Universal2D;0;5;Universal2D;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;True;1;1;False;;0;False;;0;1;False;;0;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;True;1;False;;True;3;False;;True;True;0;False;;0;False;;True;1;LightMode=Universal2D;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;174;1472,-320;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;1;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;SceneSelectionPass;0;6;SceneSelectionPass;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;2;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=SceneSelectionPass;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;175;1472,-320;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;1;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;ScenePickingPass;0;7;ScenePickingPass;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=Picking;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;176;1472,-320;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;1;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;DepthNormals;0;8;DepthNormals;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;False;;True;3;False;;False;True;1;LightMode=DepthNormalsOnly;False;False;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;177;1472,-320;Float;False;False;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;1;New Amplify Shader;2992e84f91cbeb14eab234972e07ea9d;True;DepthNormalsOnly;0;9;DepthNormalsOnly;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;False;;True;3;False;;False;True;1;LightMode=DepthNormalsOnly;False;True;9;d3d11;metal;vulkan;xboxone;xboxseries;playstation;ps4;ps5;switch;0;;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;169;2016,-304;Float;False;True;-1;2;UnityEditor.ShaderGraphUnlitGUI;0;13;FlyingAround;2992e84f91cbeb14eab234972e07ea9d;True;Forward;0;1;Forward;8;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;2;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;4;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;UniversalMaterialType=Unlit;True;5;True;12;all;0;False;True;1;1;False;;0;False;;1;1;False;;0;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;True;1;False;;True;3;False;;True;True;0;False;;0;False;;True;1;LightMode=UniversalForwardOnly;False;False;0;;0;0;Standard;21;Surface;0;0;  Blend;0;0;Two Sided;0;638519181753265070;Forward Only;0;0;Cast Shadows;1;0;  Use Shadow Threshold;0;0;GPU Instancing;1;0;LOD CrossFade;1;0;Built-in Fog;1;0;Meta Pass;0;0;Extra Pre Pass;0;0;Tessellation;0;0;  Phong;0;0;  Strength;0.5,False,;0;  Type;0;0;  Tess;16,False,;0;  Min;10,False,;0;  Max;25,False,;0;  Edge Length;16,False,;0;  Max Displacement;25,False,;0;Vertex Position,InvertActionOnDeselection;0;638519179162736438;0;10;False;True;True;True;False;False;True;True;True;False;False;;False;0
WireConnection;70;1;69;2
WireConnection;93;1;70;0
WireConnection;106;1;93;0
WireConnection;150;1;106;0
WireConnection;149;0;150;0
WireConnection;34;0;116;0
WireConnection;156;0;154;0
WireConnection;155;0;34;0
WireConnection;155;1;156;0
WireConnection;31;0;115;0
WireConnection;31;1;155;0
WireConnection;31;3;13;0
WireConnection;32;0;31;0
WireConnection;29;0;32;2
WireConnection;27;0;32;2
WireConnection;30;0;25;0
WireConnection;30;1;29;0
WireConnection;30;2;27;0
WireConnection;22;1;30;0
WireConnection;22;3;31;0
WireConnection;16;0;22;0
WireConnection;140;0;16;0
WireConnection;143;0;141;0
WireConnection;65;0;64;2
WireConnection;65;1;61;0
WireConnection;144;0;143;1
WireConnection;144;1;145;0
WireConnection;60;0;65;0
WireConnection;41;0;39;0
WireConnection;146;0;144;0
WireConnection;146;1;60;0
WireConnection;146;2;147;0
WireConnection;42;0;146;0
WireConnection;42;1;41;0
WireConnection;152;1;151;0
WireConnection;44;0;42;0
WireConnection;44;1;43;0
WireConnection;148;0;136;0
WireConnection;148;1;152;0
WireConnection;137;0;44;0
WireConnection;137;1;148;0
WireConnection;47;1;137;0
WireConnection;48;0;47;1
WireConnection;49;0;46;0
WireConnection;49;1;146;0
WireConnection;82;0;80;0
WireConnection;82;1;74;0
WireConnection;107;0;70;0
WireConnection;50;0;48;0
WireConnection;51;0;49;0
WireConnection;96;0;93;0
WireConnection;81;0;82;0
WireConnection;81;1;107;0
WireConnection;52;0;50;0
WireConnection;53;0;50;0
WireConnection;54;0;51;0
WireConnection;132;0;12;0
WireConnection;132;1;130;0
WireConnection;132;2;106;0
WireConnection;209;0;133;0
WireConnection;79;0;81;0
WireConnection;79;1;78;0
WireConnection;57;0;52;0
WireConnection;57;1;55;0
WireConnection;57;2;54;0
WireConnection;121;0;144;0
WireConnection;121;1;119;0
WireConnection;131;0;12;0
WireConnection;131;1;132;0
WireConnection;56;0;53;0
WireConnection;56;1;55;0
WireConnection;56;2;54;0
WireConnection;26;0;16;0
WireConnection;210;0;209;0
WireConnection;210;1;104;0
WireConnection;203;0;197;0
WireConnection;205;0;195;0
WireConnection;108;0;79;0
WireConnection;108;1;78;0
WireConnection;67;0;26;0
WireConnection;67;1;131;0
WireConnection;122;0;128;0
WireConnection;122;1;129;0
WireConnection;122;2;121;0
WireConnection;58;0;57;0
WireConnection;58;2;56;0
WireConnection;207;0;210;0
WireConnection;199;0;203;0
WireConnection;199;1;205;0
WireConnection;15;0;67;0
WireConnection;15;1;26;1
WireConnection;15;2;26;2
WireConnection;84;0;72;0
WireConnection;84;1;108;0
WireConnection;127;0;122;0
WireConnection;59;0;58;0
WireConnection;134;0;207;0
WireConnection;134;1;20;0
WireConnection;212;1;199;0
WireConnection;86;1;84;0
WireConnection;68;0;127;0
WireConnection;68;1;15;0
WireConnection;21;0;19;0
WireConnection;21;1;134;0
WireConnection;21;2;212;0
WireConnection;211;0;102;0
WireConnection;211;1;104;0
WireConnection;85;0;86;0
WireConnection;14;0;63;0
WireConnection;14;1;68;0
WireConnection;101;0;21;0
WireConnection;101;1;211;0
WireConnection;18;1;101;0
WireConnection;18;2;63;0
WireConnection;18;3;14;0
WireConnection;87;0;18;0
WireConnection;87;1;88;0
WireConnection;157;0;31;0
WireConnection;139;0;138;0
WireConnection;100;0;10;4
WireConnection;100;2;89;0
WireConnection;99;0;182;0
WireConnection;99;1;190;0
WireConnection;99;2;89;0
WireConnection;92;0;91;0
WireConnection;92;1;90;0
WireConnection;92;2;98;0
WireConnection;190;0;188;0
WireConnection;190;1;192;0
WireConnection;190;2;92;0
WireConnection;192;0;191;0
WireConnection;192;1;189;2
WireConnection;182;0;181;0
WireConnection;182;1;185;0
WireConnection;182;2;186;0
WireConnection;180;0;10;0
WireConnection;186;0;180;3
WireConnection;186;1;184;0
WireConnection;185;0;183;0
WireConnection;185;1;180;2
WireConnection;181;0;179;0
WireConnection;181;1;180;1
WireConnection;17;0;87;0
WireConnection;97;0;70;0
WireConnection;204;0;197;0
WireConnection;204;1;198;0
WireConnection;169;2;99;0
WireConnection;169;3;100;0
WireConnection;169;4;11;0
WireConnection;169;5;17;0
ASEEND*/
//CHKSM=39367AA73852B296CD4F780E9B7BF61575CD677E