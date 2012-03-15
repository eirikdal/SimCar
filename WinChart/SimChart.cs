using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using System.Windows.Forms;
using System.Windows.Forms.DataVisualization.Charting;

namespace WinChart
{
    public partial class SimChart : Form
    {
        private Mutex mut = new Mutex();
        private const int nRealTime = 0;
        private const int nPowerNodes = 1;
        private const int nPhev = 2;
        private const int nPhevPDF = 3;
        private const int nDayAhead = 4;
        private const int nPhevStatus = 5;
        private const int nPhevBattery = 6;        
        

        private static string[] series = { "Total", "PowerNodes", "PHEVs", "PHEV PDF", "Dayahead", "PHEV status", "PHEV battery" };
        private static int counter = 0;
        private static int nstep = 0;

        private delegate void saveImageDelegate(string fileName);
        private delegate void updateChartDelegate(int i, Double[] chart);
        private delegate void updateChartSaveImageDelegate(int i, Double[] chart, bool saveImage);
        private delegate void resetChartDelegate(int i, int j);
        private delegate void updatePDFDelegate(Double[] chart);
        private delegate void addPointDelegate(int i, Double point);
        

        void saveImageControl(string fileName)
        {


        }

        void updateChart(int i, Double[] chart, bool saveImage)
        {
            if (chart1.InvokeRequired)
            {
                chart1.Invoke(new updateChartSaveImageDelegate(updateChart), new object[] { i, chart, saveImage });
            }
            else
            {
                updateChart(i, chart);

                if (saveImage)
                    chart1.SaveImage(String.Format("C:\\SimCar\\SimCar\\data\\img\\{0}.png", counter++), ChartImageFormat.Png);
                
                for (int j = 0; j < 3; j++)
                    chart1.Series[j].Points.Clear();
            }

        }

        void updateChart(int i, Double[] chart)
        {
            if (chart1.InvokeRequired)
            {
                chart1.Invoke(new updateChartDelegate(updateChart), new object[] { i, chart });
            }
            else
            {
                if (chart1.Series[i].Points.Count > 0)
                    chart1.Series[i].Points.Clear();

                for (int j = 0; j < chart.Length; j++)
                    chart1.Series[i].Points.Add(chart[j]);
            }
        }

        void updatePDF(Double[] prob)
        {
            if (chart1.InvokeRequired)
            {
                chart1.BeginInvoke(new updatePDFDelegate(updatePDF), new object[] { prob });
            }
            else
            {
                for (int i = 0; i < prob.Length; i++)
                {
                    if (chart1.Series[nPhevPDF].Points.Count <= i)
                        chart1.Series[nPhevPDF].Points.Add(prob[i]);
                    else if (prob[i] > chart1.Series[nPhevPDF].Points[i].YValues[0])
                        chart1.Series[nPhevPDF].Points[i].SetValueY(prob[i]);
                }
            }
        }

        void addPoint(int i, Double point)
        {
            if (chart1.InvokeRequired)
            {
                chart1.BeginInvoke(new addPointDelegate(addPoint), new object[] {i, point});
            }
            else
            {
                if (chart1.Series[i].Points.Count >= 96)
                    chart1.Series[i].Points.Clear();

                chart1.Series[i].Points.Add(point);
            }
        }

        void resetChart(int from, int to)
        {
            if (chart1.InvokeRequired)
            {
                chart1.Invoke(new resetChartDelegate(resetChart), new object[] { 0, 3 });
            }
            else
            {
                for (int j = from; j < to; j++)
                    chart1.Series[j].Points.Clear();
            }
        }

        void phevStatus_Changed(object sender, EventArgs e)
        {
            Double point = (Double) sender;

            addPoint(nPhevStatus, point);
        }

        void phevBattery_Changed(object sender, EventArgs e)
        {
            Double point = (Double) sender;

            addPoint(nPhevBattery, point);
        }

        void prob_Reset(object sender, EventArgs e)
        {

        }

        void prob_Calc(object sender, EventArgs e)
        {
            Double[] chart = (Double[]) sender;

            updatePDF(chart);
        }
        
        void phev_Changed(object sender, EventArgs e)
        {
            updateChart(nPhev, (Double[])sender);
        }

        void pnode_Changed(object sender, EventArgs e)
        {
            updateChart(nPowerNodes, (Double[])sender);
        }

        void total_Changed(object sender, EventArgs e)
        {
            updateChart(nRealTime, (Double[])sender, true);
        }

        void chart_moving_average_comparison_Changed(object sender, EventArgs e)
        {
            Double[][] chart = (Double[][])sender;

            chart1.Series[0].Points.Clear();

            for (int j = 0; j < chart.Length; j++)
                chart1.Series[0].Points.Add(chart[0][j]);
        }

        void dayahead_Init(object sender, EventArgs e)
        {
            Double[] chart = (Double[])sender;

            this.chart1.Titles[0].Text = (String.Format("Iteration #{0} : Step # {0}", counter, nstep));

            updateChart(0, chart, true);
        }
        void dayahead_Changed(object sender, EventArgs e)
        {
            Double[] chart = (Double[])sender;

            this.chart1.Titles[0].Text = (String.Format("Iteration #{0} : Step # {1}", counter, nstep++));

            updateChart(4, chart);
        }

        void dayahead_Step(object sender, EventArgs e)
        {
            Double[] chart = (Double[])sender;

            this.chart1.Titles[0].Text = (String.Format("Iteration #{0} : Step # {1}", counter, nstep));

            updateChart(2, chart, true);
        }
        public SimChart()
        {
            InitializeComponent();

            const int nSim = 10;
            const int nTicks = 96;

            chart1.Titles.Add("Iteration # 0 : Step # 0");
            chart1.Titles.Add("alpha = 0.3, theta = 0.9");

            chart1.Series[0].LegendText = series[nRealTime];
            chart1.Series.Add(series[nPowerNodes]);
            chart1.Series.Add(series[nPhev]);
            chart1.Series.Add(series[nPhevPDF]);
            chart1.Series.Add(series[nDayAhead]);
            chart1.Series.Add(series[nPhevStatus]);
            chart1.Series.Add(series[nPhevBattery]);
            
            Series[] seriesArray = { chart1.Series[0], chart1.Series[1], chart1.Series[2], chart1.Series[3], chart1.Series[4], chart1.Series[5], chart1.Series[6] };

            // customize series
            seriesArray[nRealTime].ChartType = SeriesChartType.Line;
            seriesArray[nRealTime].Color = Color.Red;
            seriesArray[nRealTime].BorderWidth = 2;
            seriesArray[nRealTime].BorderDashStyle = ChartDashStyle.Solid;

            seriesArray[nPowerNodes].ChartType = SeriesChartType.Line;
            seriesArray[nPowerNodes].Color = Color.Blue;
            seriesArray[nPowerNodes].BorderWidth = 2;
            seriesArray[nPowerNodes].BorderDashStyle = ChartDashStyle.Dot;

            seriesArray[nPhev].ChartType = SeriesChartType.Line;
            seriesArray[nPhev].Color = Color.Green;
            seriesArray[nPhev].BorderWidth = 2;
            seriesArray[nPhev].BorderDashStyle = ChartDashStyle.Dash;

            seriesArray[nPhevPDF].ChartType = SeriesChartType.Line;
            seriesArray[nPhevPDF].Color = Color.Black;
            seriesArray[nPhevPDF].BorderWidth = 2;
            seriesArray[nPhevPDF].BorderDashStyle = ChartDashStyle.Dash;

            seriesArray[nDayAhead].ChartType = SeriesChartType.Line;
            seriesArray[nDayAhead].Color = Color.Sienna;
            seriesArray[nDayAhead].BorderWidth = 2;
            seriesArray[nDayAhead].BorderDashStyle = ChartDashStyle.Solid;

            seriesArray[nPhevStatus].ChartType = SeriesChartType.Line;
            seriesArray[nPhevStatus].Color = Color.Red;
            seriesArray[nPhevStatus].BorderWidth = 2;
            seriesArray[nPhevStatus].BorderDashStyle = ChartDashStyle.Solid;

            seriesArray[nPhevBattery].ChartType = SeriesChartType.Line;
            seriesArray[nPhevBattery].Color = Color.Blue;
            seriesArray[nPhevBattery].BorderWidth = 2;
            seriesArray[nPhevBattery].BorderDashStyle = ChartDashStyle.Solid;


            Sim.SimCar tSim = new Sim.SimCar(nSim, nTicks);
            tSim.Init();
            tSim.RegisterEvents();
            tSim.RegisterPhevBattery(phevBattery_Changed);
            tSim.RegisterPhevStatus(phevStatus_Changed);
            tSim.RegisterProgressTotal(total_Changed);
            tSim.RegisterProgressPhev(phev_Changed);
            tSim.RegisterProgressPnode(pnode_Changed);
            //tSim.RegisterProb(prob_Calc);
            //tSim.RegisterProbReset(prob_Reset);
            tSim.RegisterDayaheadProgress(new EventHandler(dayahead_Changed));
            //tSim.RegisterDayaheadInit(new EventHandler(dayahead_Init));
            //tSim.RegisterDayaheadStep(new EventHandler(dayahead_Step));
            //tSim.TestDayahead(nSim);
            tSim.Run();
        }
    }
}
