namespace SAILSharp.Interop {
    using System;
    using System.Text;

    internal static class Error {
        public unsafe static void HandleError(OpaqueError* error) {
            if (error != null) {
                var message = SAILAR.GetErrorMessage(error);
                SAILAR.DisposeError(error);
                var length = UIntPtr.Zero;
                var bytes = SAILAR.GetErrorMessageContents(message, &length);
                throw new ErrorException(Encoding.UTF8.GetString(new ReadOnlySpan<byte>(bytes, (int)length)));
            }
        }
    }
}
