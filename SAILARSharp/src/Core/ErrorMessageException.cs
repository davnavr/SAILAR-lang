namespace SAILARSharp.Core;

/// <summary>The exception that is thrown when an error occurs in <c>SAILARCore</c>.</summary>
public class ErrorMessageException : System.Exception {
    public ErrorMessageException(string message) : base(message) { }
}
