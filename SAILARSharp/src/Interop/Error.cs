namespace SAILARSharp.Interop;

using System;
using System.Runtime.InteropServices;

internal unsafe static class Error {
    internal readonly struct Opaque { }

    internal readonly struct OpaqueMessage { }

    [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_dispose_error", ExactSpelling = true)]
    private static extern void Dispose(Opaque* error);

    [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_dispose_error_message", ExactSpelling = true)]
    private static extern void DisposeMessage(OpaqueMessage* message);

    [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_error_message", ExactSpelling = true)]
    private static extern OpaqueMessage* GetMessage(Opaque* error);
}
