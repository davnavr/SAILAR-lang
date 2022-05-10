namespace SAILARSharp {
    using System;
    using SAILARSharp.Interop;

    public unsafe sealed class FunctionSignature : IDisposable {
        private OpaqueFunctionSignature* signature;
        private int returnTypeCount, parameterTypeCount;
        private bool disposed = false;
        private readonly object theLock = new();

        internal FunctionSignature(OpaqueFunctionSignature* signature) {
            this.signature = signature;
            UIntPtr returnCount, parameterCount;
            SAILAR.GetFunctionSignatureCounts(signature, &returnCount, &parameterCount);
            returnTypeCount = (int)returnCount;
            parameterTypeCount = (int)parameterCount;
        }

        /// <summary>
        /// Gets the number of return types for this function signature.
        /// </summary>
        public int ResultCount => returnTypeCount;

        public int ParameterCount => parameterTypeCount;

        private void ThrowIfDisposed() {
            if (disposed) {
                throw new ObjectDisposedException(null);
            }
        }

        // TODO: Return type should be a TypeSignatureIndex struct.
        public OpaqueIndex GetReturnType(int index) {
            lock (theLock) {
                OpaqueIndex returnType;
                SAILAR.GetFunctionSignatureReturnType(signature, (UIntPtr)index, &returnType);
                return returnType;
            }
        }

        public OpaqueFunctionSignature* Signature {
            get {
                ThrowIfDisposed();
                return signature;
            }
        }

        public void Dispose() {
            lock (theLock) {
                if (!disposed) {
                    GC.SuppressFinalize(this);
                    SAILAR.DisposeFunctionSignature(signature);
                    signature = null;
                    returnTypeCount = 0;
                    parameterTypeCount = 0;
                    disposed = true;
                }
            }
        }

        ~FunctionSignature() {
            Dispose();
        }
    }
}
