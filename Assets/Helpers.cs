using UnityEngine;
using System.Collections;

public class Helpers
{
	public static GameObject Raycast (Vector3 origin, Vector3 direction, int layer)
	{
		Debug.Log ("Raycast");
		RaycastHit hit;
		if (Physics.Raycast (origin, direction, out hit, Mathf.Infinity, (1 << layer), QueryTriggerInteraction.UseGlobal)) {
			Debug.Log ("hit");
			Debug.Log ("hit collider" + hit.collider);
			return hit.collider.gameObject;
		} else {
			return null;
		}
	}

	public static GameObject RaycastAll (Ray ray, int layer)
	{
		RaycastHit hit;
		if (Physics.Raycast (ray, out hit, (1 << layer))) {
			return hit.collider.gameObject;
		}

		return null;
	}
}
