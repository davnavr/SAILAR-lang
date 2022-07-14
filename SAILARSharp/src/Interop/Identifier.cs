namespace SAILARSharp.Interop;

using System;
using System.Runtime.InteropServices;

internal unsafe static class Identifier {
    internal readonly struct Opaque { }

    [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_identifier_from_utf16", ExactSpelling = true)]
    internal static extern Opaque* Create(char* contents, nuint count, out Error.Opaque* error);

    [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_dispose_identifier", ExactSpelling = true)]
    internal static extern void Dispose(Opaque* identifier);
    
    [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_identifier_contents", ExactSpelling = true)]
    internal static extern byte* GetContents(Opaque* identifier, out nuint length);
}
