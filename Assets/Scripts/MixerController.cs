using System;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.InputSystem;


public class MixerController : MonoBehaviour
{

    public InputActionReference triggerInputActionRef;

    public Transform hooks;
    
    public float rotationSpeed = 1.0f;


    void Update()
    {
        float triggerValue = triggerInputActionRef.action.ReadValue<float>();
        
        float rotation = triggerValue * rotationSpeed * Time.deltaTime;
        hooks.Rotate(Vector3.up, rotation);
    }
}