using UnityEngine;
using System.Collections;

public class Helpers {
	public static GameObject Raycast (Vector3 origin, Vector3 direction, int layer)
	{
		RaycastHit hit;
		if (Physics.Raycast (origin, direction, out hit, (1 << layer))) {
			return hit.collider.gameObject;
		} else {
			return null;
		}
	}
}
