using System;
using System.Collections.Generic;
using Sirenix.OdinInspector;
using UnityEngine;
using Random = UnityEngine.Random;


public class BeanController : MonoBehaviour
{
    // one to create and store in list
    // one to adjust, use onvalidate
    // a distribution curve might be useful, just use the usual suspects

    [SerializeField] GameObject beanPrefab;

    [SerializeField] List<GameObject> beans = new();


    [Range(0.1f, 5f)]
    [SerializeField] float distributionExpo;

    [SerializeField] bool flipDistribution;


    [Button]
    void CreateBeans(int amount)
    {
        //destroy beans
        foreach (GameObject bean in beans)
        {
            DestroyImmediate(bean);
        }

        beans.Clear();


        for (int i = 0; i < amount; i++)
        {
            GameObject newBean = Instantiate(beanPrefab, transform.position, Quaternion.identity);
            newBean.transform.parent = transform;
            beans.Add(newBean);
        }
    }


    [Button]
    void DistributeBeans()
    {
        foreach (GameObject bean in beans)
        {
            float randomY = Random.Range(0f, 1f);

            randomY = flipDistribution ? 1 - randomY : randomY;

            randomY = Mathf.Pow(randomY, distributionExpo);
            randomY *= Mathf.PI * 2;

            bean.transform.position = new Vector3(0f, randomY, 0f);
        }
    }


    void OnValidate()
    {
        DistributeBeans();
    }
}