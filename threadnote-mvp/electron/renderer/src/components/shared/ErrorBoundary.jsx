import { Component } from "react";

export class ErrorBoundary extends Component {
  constructor(props) {
    super(props);
    this.state = { error: null };
  }

  static getDerivedStateFromError(error) {
    return { error };
  }

  componentDidCatch(error, info) {
    console.error("ErrorBoundary caught:", error, info);
  }

  render() {
    if (this.state.error) {
      return (
        <div className="flex items-center justify-center h-full bg-bg">
          <div className="max-w-md p-6 rounded-lg bg-surface border border-border text-center">
            <p className="text-lg font-medium text-text mb-2">Something went wrong</p>
            <p className="text-sm text-text-secondary mb-4">{this.state.error.message}</p>
            <button
              className="px-4 py-2 text-sm font-medium bg-accent text-white rounded-md hover:bg-accent-hover transition-colors"
              onClick={() => this.setState({ error: null })}
            >
              Try again
            </button>
          </div>
        </div>
      );
    }
    return this.props.children;
  }
}
