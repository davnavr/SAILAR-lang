namespace SAILSharp {
    using System;

    public sealed class ErrorException : Exception {
        public ErrorException(string message) : base(message) {}
    }
}
