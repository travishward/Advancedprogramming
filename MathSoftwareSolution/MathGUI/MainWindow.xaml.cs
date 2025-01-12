using System;
using System.Windows;
using MathInterpreter;
using OxyPlot;
using OxyPlot.Series;
using System.Collections.Generic;

namespace MathInterpreter
{
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();
            // Initialize OxyPlot Model (T)
            FunctionPlot.Model = new PlotModel { Title = "Function Plot" };
        }

        private void Calculate_Click(object sender, RoutedEventArgs e)
        {
            ErrorOutput.Text = "";
            ResultOutput.Text = "";
            ParseTreeOutput.Text = "";

            string expression = ExpressionInput.Text;

            try
            {
                var lexResult = Lexer.lex(expression);
                if (lexResult.IsOk)
                {
                    var tokens = lexResult.ResultValue;

                    try
                    {
                        // parseStatement supports variable assignment (T)
                        var parseResult = Parser.parseStatement(tokens);
                        var finalResult = parseResult.Item1;
                        var parseTree = parseResult.Item2;

                        ResultOutput.Text = finalResult.ToString();
                        ParseTreeOutput.Text = Parser.visualizeParseTree(parseTree, "");
                    }
                    catch (Parser.ParserException pex)
                    {
                        ErrorOutput.Text = $"Parser Error: {pex.Message}";
                    }
                    catch (Exception ex)
                    {
                        ErrorOutput.Text = $"Error: {ex.Message}";
                    }
                }
                else
                {
                    ErrorOutput.Text = $"Lexer Error: {lexResult.ErrorValue}";
                }
            }
            catch (Exception ex)
            {
                ErrorOutput.Text = $"General Error: {ex.Message}";
            }
        }

        private void Help_Click(object sender, RoutedEventArgs e)
        {
            MessageBox.Show("Valid tokens: +, -, *, /, %, ^, parentheses, assignment (x=10), and implied multiplication (2x). Also trig/log functions: sin, cos, tan, log, exp.");  // (T)
        }

        private void Plot_Click(object sender, RoutedEventArgs e)
        {
            ErrorOutput.Text = "";
            ResultOutput.Text = "";
            ParseTreeOutput.Text = "";

            string functionInput = FunctionInput.Text.Trim();

            if (!functionInput.StartsWith("y=") && !functionInput.StartsWith("y ="))
            {
                ErrorOutput.Text = "Function must be in the format y = f(x)";
                return;
            }

            // Extract the expression part (right side of y = ...)
            string expression = functionInput.Substring(functionInput.IndexOf('=') + 1).Trim();

            // Parse xMin, xMax, stepSize (T)
            if (!double.TryParse(XMinInput.Text, out double xMin))
            {
                ErrorOutput.Text = "Invalid xMin value.";
                return;
            }
            if (!double.TryParse(XMaxInput.Text, out double xMax))
            {
                ErrorOutput.Text = "Invalid xMax value.";
                return;
            }
            if (!double.TryParse(StepSizeInput.Text, out double stepSize) || stepSize <= 0)
            {
                ErrorOutput.Text = "Invalid step size. Must be positive.";
                return;
            }
            if (xMin >= xMax)
            {
                ErrorOutput.Text = "xMin must be less than xMax.";
                return;
            }

            try
            {
                var xValues = new List<double>();
                var yValues = new List<double>();

                // Evaluate the expression for each x in [xMin, xMax] (T)
                for (double x = xMin; x <= xMax + 1e-9; x += stepSize)
                {
                    // Assign current x to symbol table (T)
                    MathInterpreter.SymbolTable.symbolTable["x"] = x;

                    var lexResult = Lexer.lex(expression);
                    if (!lexResult.IsOk)
                    {
                        ErrorOutput.Text = $"Lexer Error: {lexResult.ErrorValue}";
                        return;
                    }

                    var tokens = lexResult.ResultValue;
                    try
                    {
                        var parseResult = Parser.parseStatement(tokens);
                        var y = parseResult.Item1;  // numeric result
                        xValues.Add(x);
                        yValues.Add(y);
                    }
                    catch (Parser.ParserException pex)
                    {
                        ErrorOutput.Text = $"Parser Error at x = {x}: {pex.Message}";
                        return;
                    }
                    catch (Exception ex)
                    {
                        ErrorOutput.Text = $"Error at x = {x}: {ex.Message}";
                        return;
                    }
                }

                // Plot the function using OxyPlot (T)
                PlotFunction(xValues, yValues);
            }
            catch (Exception ex)
            {
                ErrorOutput.Text = $"General Error: {ex.Message}";
            }
        }

        private void PlotFunction(List<double> xValues, List<double> yValues)
        {
            var plotModel = new PlotModel { Title = "Function Plot" };
            var lineSeries = new LineSeries
            {
                Title = "y = f(x)",
                MarkerType = MarkerType.None,
                StrokeThickness = 2,
                Color = OxyColors.Blue
            };

            for (int i = 0; i < xValues.Count; i++)
            {
                lineSeries.Points.Add(new DataPoint(xValues[i], yValues[i]));
            }

            plotModel.Series.Add(lineSeries);
            FunctionPlot.Model = plotModel;
        }
    }
}
