namespace SAILARSharp.Interop {
    using System;
    using System.Text;

    internal unsafe static class Identifier_ {
        public static OpaqueIdentifier* From(string contents) {
            byte[] bytes = Encoding.UTF8.GetBytes(contents);
            OpaqueIdentifier* identifier;
            OpaqueError* error;
            fixed (byte* address = bytes) {
                identifier = SAILAR.CreateIdentifier(address, (UIntPtr)bytes.Length, &error);
            }

            Error.HandleError(error);
            return identifier;
        }

        public static string ToString(OpaqueIdentifier* identifier) {
            if (identifier == null) {
                return string.Empty;
            }

            var length = UIntPtr.Zero;
            var content = SAILAR.GetIdentifierContents(identifier, &length);
            return Encoding.UTF8.GetString(content, (int)length);
        }
    }
}
