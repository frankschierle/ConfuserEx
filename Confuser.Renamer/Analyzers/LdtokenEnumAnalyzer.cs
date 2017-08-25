using System;
using Confuser.Core;
using Confuser.Renamer.References;
using dnlib.DotNet;
using dnlib.DotNet.Emit;

namespace Confuser.Renamer.Analyzers {
	internal class LdtokenEnumAnalyzer : IRenamer {
		public void Analyze(ConfuserContext context, INameService service, ProtectionParameters parameters, IDnlibDef def) {
			var method = def as MethodDef;
			if (method == null || !method.HasBody)
				return;

			// When a ldtoken instruction reference a definition,
			// most likely it would be used in reflection and thus probably should not be renamed.
			// Also, when ToString is invoked on enum,
			// the enum should not be renamed.
			for (int i = 0; i < method.Body.Instructions.Count; i++) {
				Instruction instr = method.Body.Instructions[i];
				if (instr.OpCode.Code == Code.Ldtoken) {
					if (instr.Operand is MemberRef) {
						IMemberForwarded member = ((MemberRef)instr.Operand).ResolveThrow();
            def = member as IDnlibDef;
						if (context.Modules.Contains((ModuleDefMD)member.Module) && ((def == null) || !parameters.GetParameter(context, def, "forceRen", false)))
							service.SetCanRename(member, false, "Accessed via reflection in " + method.FullName);
					}
					else if (instr.Operand is IField) {
						FieldDef field = ((IField)instr.Operand).ResolveThrow();
						if (context.Modules.Contains((ModuleDefMD)field.Module) && !parameters.GetParameter(context, field, "forceRen", false))
							service.SetCanRename(field, false, "Accessed via reflection in " + method.FullName);
					}
					else if (instr.Operand is IMethod) {
						var im = (IMethod)instr.Operand;
						if (!im.IsArrayAccessors()) {
							MethodDef m = im.ResolveThrow();
							if (context.Modules.Contains((ModuleDefMD)m.Module) && !parameters.GetParameter(context, m, "forceRen", false))
								service.SetCanRename(method, false, "Accessed via reflection in " + method.FullName);
						}
					}
					else if (instr.Operand is ITypeDefOrRef) {
						if (!(instr.Operand is TypeSpec)) {
							TypeDef type = ((ITypeDefOrRef)instr.Operand).ResolveTypeDefThrow();
							if (context.Modules.Contains((ModuleDefMD)type.Module) &&
							    HandleTypeOf(context, service, method, i)) {
								var t = type;
								do {
								  if (!parameters.GetParameter(context, t, "forceRen", false))
								  {
								    DisableRename(context, service, parameters, t, "Accessed via typeof() in " + method.FullName, false);
								  }
								  t = t.DeclaringType;
								} while (t != null);
							}
						}
					}
					else
						throw new UnreachableException();
				}
				else if ((instr.OpCode.Code == Code.Call || instr.OpCode.Code == Code.Callvirt) &&
				         ((IMethod)instr.Operand).Name == "ToString") {
					HandleEnum(context, service, parameters, method, i);
				}
				else if (instr.OpCode.Code == Code.Ldstr) {
					TypeDef typeDef = method.Module.FindReflection((string)instr.Operand);
					if (typeDef != null)
						service.AddReference(typeDef, new StringTypeReference(instr, typeDef));
				}
			}
		}

		public void PreRename(ConfuserContext context, INameService service, ProtectionParameters parameters, IDnlibDef def) {
			//
		}

		public void PostRename(ConfuserContext context, INameService service, ProtectionParameters parameters, IDnlibDef def) {
			//
		}

		void HandleEnum(ConfuserContext context, INameService service, ProtectionParameters parameters, MethodDef method, int index) {
			var target = (IMethod)method.Body.Instructions[index].Operand;
			if (target.FullName == "System.String System.Object::ToString()" ||
			    target.FullName == "System.String System.Enum::ToString(System.String)") {
				int prevIndex = index - 1;
				while (prevIndex >= 0 && method.Body.Instructions[prevIndex].OpCode.Code == Code.Nop)
					prevIndex--;

				if (prevIndex < 0)
					return;

				Instruction prevInstr = method.Body.Instructions[prevIndex];
				TypeSig targetType;

				if (prevInstr.Operand is MemberRef) {
					var memberRef = (MemberRef)prevInstr.Operand;
					targetType = memberRef.IsFieldRef ? memberRef.FieldSig.Type : memberRef.MethodSig.RetType;
				}
				else if (prevInstr.Operand is IField)
					targetType = ((IField)prevInstr.Operand).FieldSig.Type;

				else if (prevInstr.Operand is IMethod)
					targetType = ((IMethod)prevInstr.Operand).MethodSig.RetType;

				else if (prevInstr.Operand is ITypeDefOrRef)
					targetType = ((ITypeDefOrRef)prevInstr.Operand).ToTypeSig();

				else if (prevInstr.GetParameter(method.Parameters) != null)
					targetType = prevInstr.GetParameter(method.Parameters).Type;

				else if (prevInstr.GetLocal(method.Body.Variables) != null)
					targetType = prevInstr.GetLocal(method.Body.Variables).Type;

				else
					return;

				ITypeDefOrRef targetTypeRef = targetType.ToBasicTypeDefOrRef();
				if (targetTypeRef == null)
					return;

				TypeDef targetTypeDef = targetTypeRef.ResolveTypeDefThrow();
				if (targetTypeDef != null && targetTypeDef.IsEnum && context.Modules.Contains((ModuleDefMD)targetTypeDef.Module))
					DisableRename(context, service, parameters, targetTypeDef, "ToString() is called");
			}
		}

		bool HandleTypeOf(ConfuserContext context, INameService service, MethodDef method, int index) {
			if (index + 1 >= method.Body.Instructions.Count)
				return true;

			var gtfh = method.Body.Instructions[index + 1].Operand as IMethod;
			if (gtfh == null || gtfh.FullName != "System.Type System.Type::GetTypeFromHandle(System.RuntimeTypeHandle)")
				return true;

			if (index + 2 < method.Body.Instructions.Count) {
				Instruction instr = method.Body.Instructions[index + 2];
				var operand = instr.Operand as IMethod;
				if (instr.OpCode == OpCodes.Newobj && operand.FullName == "System.Void System.ComponentModel.ComponentResourceManager::.ctor(System.Type)")
					return false;
				if (instr.OpCode == OpCodes.Call || instr.OpCode == OpCodes.Callvirt) {
					switch (operand.DeclaringType.FullName) {
						case "System.Runtime.InteropServices.Marshal":
							return false;
						case "System.Type":
							if (operand.Name.StartsWith("Get") || operand.Name == "InvokeMember")
								return true;
							if (operand.Name == "get_AssemblyQualifiedName" ||
							    operand.Name == "get_FullName" ||
							    operand.Name == "get_Namespace")
								return true;
							return false;
						case "System.Reflection.MemberInfo":
							return operand.Name == "get_Name";
						case "System.Object":
							return operand.Name == "ToString";
					}
				}
			}
			if (index + 3 < method.Body.Instructions.Count) {
				Instruction instr = method.Body.Instructions[index + 3];
				var operand = instr.Operand as IMethod;
				if (instr.OpCode == OpCodes.Call || instr.OpCode == OpCodes.Callvirt) {
					switch (operand.DeclaringType.FullName) {
						case "System.Runtime.InteropServices.Marshal":
							return false;
					}
				}
			}

			return false;
		}

		void DisableRename(ConfuserContext context, INameService service, ProtectionParameters parameters, TypeDef typeDef, string reason, bool memberOnly = true) {
			service.SetCanRename(typeDef, false, reason);

		  foreach (MethodDef m in typeDef.Methods)
		  {
		    if (!parameters.GetParameter(context, m, "forceRen", false))
		    {
		      service.SetCanRename(m, false, reason);
		    }
		  }

		  foreach (FieldDef field in typeDef.Fields)
		  {
		    if (!parameters.GetParameter(context, field, "forceRen", false))
		    {
		      service.SetCanRename(field, false, reason);
		    }
		  }

		  foreach (PropertyDef prop in typeDef.Properties)
		  {
		    if (!parameters.GetParameter(context, prop, "forceRen", false))
		    {
		      service.SetCanRename(prop, false, reason);
		    }
		  }

		  foreach (EventDef evt in typeDef.Events)
		  {
		    if (!parameters.GetParameter(context, evt, "forceRen", false))
		    {
		      service.SetCanRename(evt, false, reason);
		    }
		  }

		  foreach (TypeDef nested in typeDef.NestedTypes)
		  {
		    DisableRename(context, service, parameters, nested, reason, false);
		  }
		}
	}
}