/*

  [System.Reflection.Assembly]::LoadFrom("c:\\dinoch\\dev\\dotnet\\CsdeUtilities.dll");
  [Ionic.Csde.Utilities]::GetTypeInfo("System.Data.SqlClient.SqlCommand","System.Data, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089");

  [Ionic.Csde.Utilities]::GetTypeInfo("System.Xml.XmlReader","System.Xml, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089");


  [Ionic.Csde.Utilities]::GetTypeInfo("System.DateTime","mscorlib, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089");

  [Ionic.Csde.Utilities]::GetTypeInfo("System.IO.DriveInfo","mscorlib, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089");

  [Ionic.Csde.Utilities]::QualifyType("System.DateTime");

  [Ionic.Csde.Utilities]::QualifyType("System.Xml.XmlReader");

  [Ionic.Csde.Utilities]::QualifyType("System.This.Type.Does.Not.Exist");

  ;(debug-on-entry 'csde-complete-referenced-assemblies-list)

  (semantic-brute-find-tag-by-class 'using)

*/

// CsdeUtilities.cs
// ------------------------------------------------------------------
//
//
// This thing defines an assembly that runs in an emacs inferior shell,
// actually in a powershell.  When in CSDE mode, asking for a
// code-completion on a line of code will sends a query to the inferior
// shell.  The logic in this assembly will then reflect on the specified
// type, and return the type information to the CSDE mode, allowing
// "code completion".
//
// compile with: 
//  csc.exe  /target:library  /debug /out:CsdeUtilities.dll  CsdeUtilities.cs
//
// 
// Author: Dinoch
// built on host: DINOCH-2
// Created Mon Apr 21 08:40:47 2008
//
// last saved: 
// Time-stamp: <Tuesday, April 22, 2008  08:03:12  (by dinoch)>
// ------------------------------------------------------------------
//
// Copyright (c) 2008 by Dino Chiesa
// All rights reserved!
// 
//
// ------------------------------------------------------------------

using System; 
using System.IO; 
using System.Collections; 
using System.Collections.Generic; 
using System.Reflection; 


namespace Ionic.Csde
{

  public class Utilities
  {
    public const string DotNetDir= "c:\\.net2.0";

    public static String GetTypeInfo(String TypeName, String AssemblyName)
    {
      try 
      {

	// Load from a strongname, eg
	// "System.Windows.Forms, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"

	Assembly a= Assembly.Load(AssemblyName); 

	if ((a == null) && (System.IO.File.Exists(AssemblyName)))
	  a= Assembly.LoadFrom(AssemblyName); 

	if (a == null) 
	{
	  System.Console.Error.WriteLine("Cannot load that assembly");
	  return null;
	}

	Ionic.Csde.TypeInfo ti= new Ionic.Csde.TypeInfo(a, TypeName);
	return ti.TypeInfoSexp();
      }
      catch(TypeLoadException e2)
      {
	Console.Error.WriteLine("TypeLoadException: Could not load type: \"{0}\"\n{1}", TypeName, e2);
	return null;
      }
    }



    private static System.Type TryLoad(String theTypeName, String AssemblyName)
    {
      System.Type t= null; 
      Assembly a= null;

      int p= theTypeName.LastIndexOf(',');
      if (p>0) 
      {
	// smells like a fully-qualified name
	a= Assembly.Load(AssemblyName);

      }
      else if (AssemblyName.EndsWith(".dll"))
      {
	if (System.IO.File.Exists(AssemblyName))
	  a= Assembly.LoadFrom(AssemblyName) ;
      }
      else 
      {
	// try finding a DLL by that name in the c:\.net2.0 directory
	string GuessedDll = String.Format("{0}\\{1}.dll", DotNetDir, AssemblyName);
	if (System.IO.File.Exists(GuessedDll))
	  a= Assembly.LoadFrom(GuessedDll) ;
      }

      if (a != null) 
	t = a.GetType(theTypeName, false, true);

      return t;  // maybe null
    }




    public static String QualifyType(String TypeName)
    {
      // There is no assembly name given, so we apply a heuristic here.  
      // Guess the assembly name: 
      //  (1) mscorlib
      //  (2) System
      //  (3) an assembly name derived from a fully qualified typename
      // If none of those succeeds, then fail.
      
      try 
      {
	Type t = TryLoad(TypeName, "mscorlib");
	if (t==null)
	  t = TryLoad(TypeName, "System");

	if (t==null)
	{
	  int p= TypeName.LastIndexOf('.');
	  if (p>0)
	    t= TryLoad(TypeName, TypeName.Substring(0,p));
	}

	string result= (t==null) ? "nil" :
	  String.Format("(list \"{0}\" \"{1}\")", t.FullName, t.Assembly.FullName);

	return result;
      }
      catch(TypeLoadException e2)
      {
	Console.Error.WriteLine("TypeLoadException: Could not load type: \"{0}\"\n{1}", TypeName, e2);
	return null;
      }
    }



//     public static String TypeExists(String TypeName)
//     {
//       // There is no assembly name given, so we apply a heuristic here.  
//       // Guess the assembly name: 
//       //  (1) mscorlib
//       //  (2) System
//       //  (3) an assembly name derived from a fully qualified typename
//       // If none of those succeeds, then fail.
      
//       try 
//       {
// 	Type t = TryLoad(TypeName, "mscorlib");
// 	if (t==null)
// 	  t = TryLoad(TypeName, "System");

// 	if (t==null)
// 	{
// 	  int p= TypeName.LastIndexOf('.');
// 	  t= TryLoad(TypeName, TypeName.Substring(0,p));
// 	}

// 	return (t == null) ? "// Error: Could not find that type." :  t.AssemblyQualifiedName.ToString();

//       }
//       catch(TypeLoadException e2)
//       {
// 	Console.Error.WriteLine("TypeLoadException: Could not load type: \"{0}\"\n{1}", TypeName, e2);
// 	return null;
//       }
//     }



    //     public static String TypeExists(String TypeName, String AssemblyName)
    //     {
    //       try 
    //       {
    // 	// Try loading from a strongname, eg
    // 	// "System.Windows.Forms, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"
    // 	Assembly a= Assembly.Load(AssemblyName); 

    // 	// If that did not work, try loading from a DLL, eg c:\.net2.0\system.xml.dll
    // 	if (a == null) 
    // 	  a= Assembly.LoadFrom(AssemblyName); 

    // 	if (a == null) 
    // 	{
    // 	  System.Console.Error.WriteLine("Cannot load that assembly");
    // 	  return null;
    // 	}

    // 	Type t = a.GetType(TypeName, false, true);
	
    // 	return (t == null) ? "// Error: Could not find that type." :  t.AssemblyQualifiedName.ToString();

    //       }
    //       catch(TypeLoadException e2)
    //       {
    // 	Console.Error.WriteLine("TypeLoadException: Could not load type: \"{0}\"\n{1}", TypeName, e2);
    // 	return null;
    //       }
    //     }


  }



  public class TypeInfo 
  {
    internal TypeInfo(Assembly a, string TypeName)
    {
      mt = a.GetType(TypeName, false, true);
      if ( mt == null ) 
	throw new Exception(String.Format("Cannot get that type ({0})", TypeName));
    }

 
    internal String TypeInfoSexp()
    {
      var result= new System.Text.StringBuilder();
      result.Append("(list \"").Append(mt.FullName).Append("\" 'type (list ");

      // properties
      foreach (string s in GetPropertiesInfo())
	result.Append(s);

      result.Append(") (list");

      // methods 
      foreach (string s in GetMethodsInfo())
	result.Append(s);


      // fields?

      result.Append("))");
      //       Emit("Methods", ti.GetMethodsHtml(), false);
      //       Emit("Properties", ti.GetPropertiesHtml(), false);
      //       Emit("Fields", ti.GetFieldsHtml(), false);
      //Emit("Events", ti.GetEventsHtml(), false);


//       foreach (string s in GetConstructorsInfo())
// 	result.Append(s);


      return result.ToString();
    }

    //     public String LastError { get { return lastError;}  }
    //     public Type   TheType   { get { return mt;      }  }
    //     public String FQN       { get { return (mt!=null) ? mt.AssemblyQualifiedName: null; }  }
    //     public String Name      { get { return (mt!=null) ? mt.Name: null;}  }
    //     public String FullName  { get { return (mt != null) ? mt.FullName : null;}  }
    //     public String Status    { get { return status;}  }

    //     public String Href 
    //     {
    //       set { theHref= value; }
    //       get { return theHref; }
    //     }

    private static Type ReferencedType(Type t)
    {
      return (t.HasElementType) ? ReferencedType(t.GetElementType()) : t ;
    }

    private static String EmitOneType(Type t1)
    {
      return t1.ToString();
    }


    private static String EmitOneTypeWithInterfaces(Type t1)
    {
      Type t= ReferencedType(t1);
      var result= new System.Text.StringBuilder();
      result.Append(t1.ToString());

      Type[] it=  t.GetInterfaces();
      if (it.Length > 0 )
      {
	int j=0;
	for (int i=0; i < it.Length; i++)
	{
	  if ((t.BaseType!=null) && !(it[i].IsAssignableFrom(t.BaseType)))
	  {
	    if (j==0) result.Append(" : ");
	    else result.Append(", ");
	    result.Append(EmitOneType(it[i]));
	    j++;
	  }
	}
      }

      return result.ToString();
    }



    private string ParamString(System.Reflection.ParameterInfo[] pi, bool isProperty)
    {
      if (pi.Length==0) return "nil";

      var sb=  new System.Text.StringBuilder("(list ");
      int j;
      String openBracket= "(";
      String closeBracket= ")";
      if (isProperty)
      {
	openBracket= "[";
	closeBracket= "]";
      }


      for (j=0; j < pi.Length; j++)
      {
	//	ParameterAttributes Attributes =  pi[j].Attributes;
	//if (Attributes != ParameterAttributes.None) sb.Append("[").Append(Attributes.ToString()).Append("] "); // eg, In,Out,Optional

	sb.Append("\"").Append(pi[j].ParameterType.ToString()).Append("\" ");
      }
      if (isProperty)
      {
	if (j!=0) sb.Append(closeBracket);
      }
      else 
      {
	if (j==0) sb.Append(openBracket);
	sb.Append(closeBracket);
      }

	  return sb.ToString();
    }


    private static String EmitAttributes(Type t) {
      System.Text.StringBuilder result= new System.Text.StringBuilder();
      if (t.IsSerializable)   result.Append("[serializable]<br>\n "); 
      if (t.IsPublic)         result.Append("public "); 
      if (t.IsAbstract)       result.Append("abstract "); 
      if (t.IsSealed)         result.Append("sealed "); 
      if (t.IsClass)          result.Append("class "); 
      if (t.IsEnum)           result.Append("enum "); 
      if (t.IsInterface)      result.Append("interface "); 
      return result.ToString();
    }



    //     public String GetDeclarationHtml()
    //     {
    //       System.Text.StringBuilder sb1 = new System.Text.StringBuilder();
    //       sb1.Append(EmitAttributes(mt)); 
    //       sb1.Append("  ").Append(mt.Name).Append("()");

    //       Type[] it= mt.GetInterfaces();
    //       if (it.Length > 0 )
    //       {
    // 	int j=0;
    // 	for (int i=0; i < it.Length; i++) {
    // 	  if ((mt.BaseType!=null) && !(it[i].IsAssignableFrom(mt.BaseType)))
    // 	  {
    // 	    if (j==0) sb1.Append(" : ");
    // 	    else sb1.Append(", ");
    // 	    sb1.Append(EmitOneType(it[i]));
    // 	    j++;
    // 	  }
    // 	}
    //       }

    //       return sb1.ToString();
    //     }



    private String[] GetConstructorsInfo()
    {
      if (mt==null) return null;
      System.Reflection.ConstructorInfo[] ci= mt.GetConstructors();
      if (ci==null) return null;
      if (ci.Length==0) return null;

      var a= new List<String>(); 
      System.Text.StringBuilder sb1;
      foreach (ConstructorInfo c in ci) {

	sb1 = new System.Text.StringBuilder();
	sb1.Append("(list \"").Append(mt.Name).Append("\" 'function ");  // c.Name == .ctor

	sb1.Append(ParamString(c.GetParameters(), false));

	sb1.Append(" (list '(constructor . t) "); 

	sb1.Append(MethodBaseModifiers(c));
	    
	sb1.Append(") nil)");
	  
	a.Add(sb1.ToString());
      }
      return a.ToArray(); 
    }



    private String PropertyModifiers(PropertyInfo p)
    {
      var sb= new System.Text.StringBuilder("(cons 'typemodifiers (list ");
      System.Reflection.MethodInfo mi= null;

      if (p.GetGetMethod() != null) 
      {
	mi= p.GetGetMethod();
	if (p.GetSetMethod() == null)
	  sb.Append("\"readonly\" ");
      }
      else if (p.GetSetMethod() != null) {
	mi= p.GetSetMethod();
	sb.Append("\"writeonly\" "); 
      }

      if (mi != null) {
	if (mi.IsPublic)
	  sb.Append("\"public\" "); 
	if (mi.IsPrivate)
	  sb.Append("\"private\" "); 
	if (mi.IsFamily)
	  sb.Append("\"protected\" "); 

	if (mi.IsStatic)
	  sb.Append("\"static\" "); 
      }

      sb.Append("))");
      return sb.ToString();
    }

    private String[] GetPropertiesInfo()
    {
      if (mt==null) return null;
      System.Reflection.PropertyInfo[] pi= mt.GetProperties();
      if (pi==null) return null;
      if (pi.Length==0) return null;

      var a= new List<String>(); 
      System.Text.StringBuilder sb1;
      foreach (PropertyInfo p in pi) {

	sb1 = new System.Text.StringBuilder();
	sb1.Append("(list \"").Append(p.Name).Append("\" 'property ");

	sb1.Append("\"").Append(p.PropertyType.ToString()).Append("\" ");

	sb1.Append(PropertyModifiers(p));
	    
	sb1.Append(")");
	  
	a.Add(sb1.ToString());
      }
      return a.ToArray(); 
    }



    private String EmitMethodAttrs(MethodInfo m) {
      System.Text.StringBuilder result= new System.Text.StringBuilder();

      if (m.IsPublic) 
	result.Append("public "); 

      if (m.IsFamily) 
	result.Append("protected "); 

      if (m.IsPrivate) 
	result.Append("private "); 

      if (m.IsAbstract) 
	result.Append("abstract "); 

      if (m.IsStatic) 
	result.Append("static "); 

      if (m.IsFinal) 
	result.Append("final "); 

      return result.ToString();
    }
 


    public String[] GetMethodsInfo()
    {
      System.Reflection.MethodInfo[] mi= mt.GetMethods();
      System.Array.Sort(mi,new MpfComparer());
      var a= new List<String>();
      System.Text.StringBuilder sb1;
      foreach (MethodInfo m in mi)
      {
	sb1= null;
	  // special name denotes???? I don't know
	  if (!m.IsSpecialName) 
	  {
	    // it's a method: 
	    sb1 = new System.Text.StringBuilder(" (list \"");
	    sb1.Append(m.Name).Append("\" 'function ");
	    
	    sb1.Append("\"").Append(m.ReturnType.ToString()).Append("\" ");

	    sb1.Append(ParamString(m.GetParameters(), false));

	    sb1.Append(MethodBaseModifiers(m));
	    
	    sb1.Append(")");
	  }

	if (sb1 != null)
	  a.Add(sb1.ToString());
      }
      return a.ToArray(); 
    }



    private string MethodBaseModifiers(MethodBase mi)
    {
      var sb= new System.Text.StringBuilder(" (cons 'typemodifiers (list ");

	if (mi.IsFinal)
	  sb.Append("\"sealed\" "); 

	if (mi.IsPublic)
	  sb.Append("\"public\" "); 
	if (mi.IsPrivate)
	  sb.Append("\"private\" "); 
	if (mi.IsFamily)
	  sb.Append("\"protected\" "); 

	if (mi.IsStatic)
	  sb.Append("\"static\" "); 

	sb.Append("))");
	return sb.ToString();
    }


    //     private void BuildClassHierarchy(Type t) {
    //       classHierarchy.Push(t); 
    //       if (t.BaseType != null) 
    // 	BuildClassHierarchy(t.BaseType); 
    //     }

  
    //     public String GetHierarchyHtml() {
    //       if (mt == null) return null; 
    //       classHierarchy= new Stack();
    //       BuildClassHierarchy(mt); 
    //       int c= classHierarchy.Count; 
    //       System.Text.StringBuilder sb= new System.Text.StringBuilder("\n");
    //       int i=0;
    //       while (classHierarchy.Count != 0) {
    // 	sb.Append("<div class='elt'>");
    // 	if (i!=0) sb.Append("+&nbsp;");
    // 	sb.Append(EmitOneTypeWithInterfaces((Type)classHierarchy.Pop())).Append("\n"); 
    // 	i++;
    //       }
    //       for (i=0; i < c; i++) sb.Append("</div>"); 
   
    //       return sb.ToString();
    //     }


 

    //     public String[] GetPropertiesHtml() {
    //       if (mt == null) return null;
    //       System.Reflection.PropertyInfo[] pi= mt.GetProperties();
    //       System.Array.Sort(pi,new myComparer());  // by name
    //       ArrayList a= new ArrayList();
    //       System.Text.StringBuilder sb1;
    //       foreach (PropertyInfo p in pi) {
    // 	try{
    // 	  sb1 = new System.Text.StringBuilder();
    // 	  sb1.Append("  ").Append(PropAttrsString(p));
    // 	  sb1.Append("  ").Append(EmitOneType(p.PropertyType));
    // 	  sb1.Append("  <b>").Append(p.Name).Append("</b>"); 
    // 	  AppendParams(sb1,p.GetIndexParameters(), true);
    // 	}
    // 	catch(Exception e){
    // 	  a.Add(e.Message);
    // 	  continue;
    // 	}
    // 	a.Add(sb1.ToString());
    //       }
    //       return (String[]) a.ToArray(typeof(String)); 
    //     }



    private static String FieldAttrsString(FieldInfo f)
    {
      System.Text.StringBuilder result= new System.Text.StringBuilder();

      if (f.IsPublic) 
	result.Append("public "); 

      if (f.IsFamily) 
	result.Append("protected "); 

      if (f.IsPrivate) 
	result.Append("private "); 

      if (f.IsLiteral) 
	result.Append("const "); 

      if (f.IsStatic) 
	result.Append("static "); 

      return result.ToString();
    }
 

    public String[] GetFieldsInfo()
    {
      if (mt == null) return null;
      System.Reflection.FieldInfo[] fi= mt.GetFields();
      System.Array.Sort(fi,new MpfComparer());  // by name
      ArrayList a= new ArrayList();
      System.Text.StringBuilder sb1;
      foreach (FieldInfo f in fi) {
	try{
	  sb1 = new System.Text.StringBuilder();
	  sb1.Append("  ").Append(FieldAttrsString(f));
	  sb1.Append("  ").Append(EmitOneType(f.FieldType));
	  sb1.Append("  <b>").Append(f.Name).Append("</b>"); 
	}
	catch(Exception e){
	  a.Add(e.Message);
	  continue;
	}
	a.Add(sb1.ToString());
      }
      return (String[]) a.ToArray(typeof(String)); 
    }


    private System.Type mt;
    // private Stack classHierarchy; 

  }



  public class MpfComparer : System.Collections.IComparer 
  {
    static Type mi= typeof(MethodInfo);
    static Type pi= typeof(PropertyInfo);
    static Type fi= typeof(FieldInfo);

    public int Compare (Object x, Object y)
    {
      if (mi.IsInstanceOfType(x)) {
	MethodInfo i1= (MethodInfo) x;
	MethodInfo i2= (MethodInfo) y;
	return i1.Name.CompareTo(i2.Name);
      }

      if (pi.IsInstanceOfType(x)) {
	PropertyInfo i1= (PropertyInfo) x;
	PropertyInfo i2= (PropertyInfo) y;
	return i1.Name.CompareTo(i2.Name);
      }
      if (fi.IsInstanceOfType(x)) {
	FieldInfo i1= (FieldInfo) x;
	FieldInfo i2= (FieldInfo) y;
	return i1.Name.CompareTo(i2.Name);
      }

      return 0;
    }
  }

}
