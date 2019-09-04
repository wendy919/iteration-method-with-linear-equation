using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;

namespace iteration_method_with_linear_equations
{
    public partial class Form1 : Form
    {
        int times;
        public Form1()
        {
            InitializeComponent();
        }

        private void Form1_Load(object sender, EventArgs e)
        {

        }
        public void change(double[,] a, int i, int j, int n)
        {
            int k;
            double tmp;
            if (a[j, i] == 0)
                return;
            for (k = 0; k < n; ++k)
            {
                tmp = a[i, k];
                a[i, k] = a[j, k];
                a[j, k] = tmp;
            }
            return;
        }

        public int jacobi(double[,] a, double[] b, double[] x, int n, double e, int t)
        {
            int i, j;
            double p;
            double[] xi;
            xi = new double[n];
            if (e < 0)
                return 1;
            times = 0;
            while (t == 0 || times < t)
            {
                for (i = 0; i < n; ++i)
                    xi[i] = x[i];
                for (i = 0; i < n; ++i)
                {
                    x[i] = b[i];
                    j = i + 1;
                    while (j < n && a[i, i] == 0)
                    {
                        change(a, i, j, n);
                        ++j;
                    }
                    if (a[i, i] == 0)
                        return 1;
                    for (j = 0; j < n; ++j)
                    {
                        if (j != i)
                            x[i] -= a[i, j] * xi[j];
                    }
                    x[i] /= a[i, i];
                }
                p = System.Math.Abs(x[0] - xi[0]);
                for (i = 1; i < n; ++i)
                {
                    if (System.Math.Abs(x[i] - xi[i]) > p)
                        p = System.Math.Abs(x[i] - xi[i]);
                }
                ++times;
                if (double.IsInfinity(p) || double.IsNaN(p))
                    return -1;
                if (e != 0 && p < e)
                    break;
            }
            return 0;
        }


        public int Gauss_Seidel(double[,] a, double[] b, double[] x, int n, double e, int t)
        {
            double[]  xi;
            int i, j;
            double p;
            xi = new double[n];
            if (e < 0)
                return 1;
            times = 0;
            while (t == 0 || times < t)
            {
                for (i = 0; i < n; ++i)
                    xi[i] = x[i];
                for (i = 0; i < n; ++i)
                {
                    x[i] = b[i];
                    j = i + 1;
                    while (j < n && a[i, i] == 0)
                    {
                        change(a, i, j, n);
                        ++j;
                    }
                    if (a[i, i] == 0)
                        return 1;
                    for (j = 0; j < n; ++j)
                    {
                        if (j < i)
                            x[i] -= a[i, j] * x[j];
                        if (j > i)
                            x[i] -= a[i, j] * xi[j];
                    }
                    x[i] /= a[i, i];
                }
                p = System.Math.Abs(x[0] - xi[0]);
                for (i = 1; i < n; ++i)
                {
                    if (System.Math.Abs(x[i] - xi[i]) > p)
                        p = System.Math.Abs(x[i] - xi[i]);
                }
                ++times;
                if (double.IsInfinity(p) || double.IsNaN(p))
                    return -1;
                if (e != 0 && p < e)
                    break;
            }
            return 0;
        }

        public int sor(double[,] a, double[] b, double[] x, int n, double e, double w, int t)
        {
            double[]  xi;
            int i, j;
            double p;
            xi = new double[n];
            if (e < 0)
                return 1;
            times = 0;
            while (t == 0 || times < t)
            {
                for (i = 0; i < n; ++i)
                    xi[i] = x[i];
                for (i = 0; i < n; ++i)
                {
                    x[i] = b[i];
                    j = i + 1;
                    while (j < n && a[i, i] == 0)
                    {
                        change(a, i, j, n);
                        ++j;
                    }
                    if (a[i, i] == 0)
                        return 1;
                    for (j = 0; j < n; ++j)
                    {
                        if (j < i)
                            x[i] -= a[i, j] * x[j];
                        if (j > i)
                            x[i] -= a[i, j] * xi[j];
                    }
                    x[i] /= a[i, i];
                    x[i] = (1 - w) * xi[i] + w * x[i];
                }
                p = System.Math.Abs(x[0] - xi[0]);
                for (i = 1; i < n; ++i)
                {
                    if (System.Math.Abs(x[i] - xi[i]) > p)
                        p = System.Math.Abs(x[i] - xi[i]);
                }
                ++times;
                if (double.IsInfinity(p) || double.IsNaN(p))
                    return -1;
                if (e != 0 && p < e)
                    break;
            }
            return 0;
        }

       
        private void button1_Click(object sender, EventArgs ev)
        {
            double[,] a;
            double[] x, b;
            int n, i, j, tag, t = 0;
            double e = 0;
            string[] r, rd, rx;
            string str_r, str_n, str_e, str_x, str_t;
            str_r = richTextBox1.Text;
            str_n = textBox1.Text;
            str_e = textBox2.Text;
            str_x = textBox4.Text;
            str_t = textBox5.Text;
            n = int.Parse(str_n);
            r = (str_r.TrimEnd()).Split('\n');
            if (r.Length != n)
            {
                richTextBox2.Text = "input error!\n";
                return;
            }
            if (str_e.Length == 0)
            {
                if (str_t.Length == 0)
                {
                    tag = 0;
                    e = 0.0001;
                }
                else
                {
                    tag = 1;
                    t = int.Parse(str_t);
                }
            }
            else
            {
                if (str_t.Length == 0)
                {
                    tag = 0;
                    e = double.Parse(str_e);
                }
                else
                {
                    richTextBox2.Text = "input error!\n";
                    return;
                }
            }
            a = new double[n, n];
            b = new double[n];
            x = new double[n];
            for (i = 0; i < r.Length; ++i)
            {
                rd = r[i].Split(' ');
                if (rd.Length != n + 1)
                {
                    richTextBox2.Text = "input error!\n";
                    return;
                }
                for (j = 0; j < n; ++j)
                    a[i, j] = double.Parse(rd[j]);
                b[i] = double.Parse(rd[n]);
            }
            if (str_x.Length != 0)
            {
                rx = str_x.Split(',');
                if (rx.Length != n)
                {
                    richTextBox2.Text = "input error!\n";
                    return;
                }
                for (i = 0; i < n; ++i)
                    x[i] = double.Parse(rx[i]);
            }
            else
            {
                for (i = 0; i < n; ++i)
                    x[i] = 0;
            }
            if (tag == 0)
            {
                tag = jacobi(a, b, x, n, e, 0);
                if (tag > 0)
                {
                    richTextBox2.Text = "data error!\n";
                    return;
                }
            }
            else if (tag == 1)
            {
                tag = jacobi(a, b, x, n, 0, t);
                if (tag > 0)
                {
                    richTextBox2.Text = "data error!\n";
                    return;
                }
            }
            StringBuilder mystring = new StringBuilder("result:\n");
            if (tag < 0)
                mystring.Append("Jacobi迭代矩阵不收敛\n");
            else
            {
                for (i = 0; i < n; ++i)
                {
                    mystring.Append("x" + i.ToString() + ": ");
                    mystring.Append(x[i].ToString("f4") + "   ");
                }
                if (t == 0)
                    mystring.Append("\n迭代次数： " + times.ToString() + "\n");
            }
            richTextBox2.Text = mystring.ToString();
        }

        private void button2_Click(object sender, EventArgs ev)
        {
            double[,] a;
            double[] x, b;
            int n, i, j, tag, t = 0;
            double e = 0;
            string[] r, rd, rx;
            string str_r, str_n, str_e, str_x, str_t;
            str_r = richTextBox1.Text;
            str_n = textBox1.Text;
            str_e = textBox2.Text;
            str_x = textBox4.Text;
            str_t = textBox5.Text;
            n = int.Parse(str_n);
            r = (str_r.TrimEnd()).Split('\n');
            if (r.Length != n)
            {
                richTextBox3.Text = "input error!\n";
                return;
            }
            if (str_e.Length == 0)
            {
                if (str_t.Length == 0)
                {
                    tag = 0;
                    e = 0.0001;
                }
                else
                {
                    tag = 1;
                    t = int.Parse(str_t);
                }
            }
            else
            {
                if (str_t.Length == 0)
                {
                    tag = 0;
                    e = double.Parse(str_e);
                }
                else
                {
                    richTextBox3.Text = "input error!\n";
                    return;
                }
            }
            a = new double[n, n];
            b = new double[n];
            x = new double[n];
            for (i = 0; i < r.Length; ++i)
            {
                rd = r[i].Split(' ');
                if (rd.Length != n + 1)
                {
                    richTextBox3.Text = "input error!\n";
                    return;
                }
                for (j = 0; j < n; ++j)
                    a[i, j] = double.Parse(rd[j]);
                b[i] = double.Parse(rd[n]);
            }
            if (str_x.Length != 0)
            {
                rx = str_x.Split(',');
                if (rx.Length != n)
                {
                    richTextBox3.Text = "input error!\n";
                    return;
                }
                for (i = 0; i < n; ++i)
                    x[i] = double.Parse(rx[i]);
            }
            else
            {
                for (i = 0; i < n; ++i)
                    x[i] = 0;
            }
            if (tag == 0)
            {
                tag = Gauss_Seidel(a, b, x, n, e, 0);
                if (tag > 0)
                {
                    richTextBox3.Text = "data error!\n";
                    return;
                }
            }
            else if (tag == 1)
            {
                tag = Gauss_Seidel(a, b, x, n, 0, t);
                if (tag > 0)
                {
                    richTextBox3.Text = "data error!\n";
                    return;
                }
            }
            StringBuilder mystring = new StringBuilder("result:\n");
            if (tag < 0)
                mystring.Append("Gauss_Seidel迭代矩阵不收敛\n");
            else
            {
                for (i = 0; i < n; ++i)
                {
                    mystring.Append("x" + i.ToString() + ": ");
                    mystring.Append(x[i].ToString("f4") + "   ");
                }
                if (tag == 0)
                    mystring.Append("\n迭代次数： " + times.ToString() + "\n");             
            }
            richTextBox3.Text = mystring.ToString();
        }


        private void button3_Click(object sender, EventArgs ev)
        {
            double[,] a;
            double[] x, b;
            int n, i, j, tag, t = 1;
            double e = 0, w;
            string[] r, rd, rx;
            string str_r, str_n, str_e, str_x, str_w, str_t;
            str_r = richTextBox1.Text;
            str_n = textBox1.Text;
            str_e = textBox2.Text;
            str_w = textBox3.Text;
            str_x = textBox4.Text;
            str_t = textBox5.Text;
            n = int.Parse(str_n);
            r = (str_r.TrimEnd()).Split('\n');
            if (r.Length != n)
            {
                richTextBox4.Text = "input error!()\n";
                return;
            }
            if (str_w.Length == 0)
                w = 1.0;
            else
                w = double.Parse(str_w);
            if (str_e.Length == 0)
            {
                if (str_t.Length == 0)
                {
                    tag = 0;
                    e = 0.0001;
                }
                else
                {
                    tag = 1;
                    t = int.Parse(str_t);
                }
            }
            else
            {
                if (str_t.Length == 0)
                {
                    tag = 0;
                    e = double.Parse(str_e);
                }
                else
                {
                    richTextBox4.Text = "input error!()\n";
                    return;
                }
            }
            a = new double[n, n];
            b = new double[n];
            x = new double[n];
            for (i = 0; i < r.Length; ++i)
            {
                rd = r[i].Split(' ');
                if (rd.Length != n + 1)
                {
                    richTextBox4.Text = "input error! ()\n";
                    return;
                }
                for (j = 0; j < n; ++j)
                    a[i, j] = double.Parse(rd[j]);
                b[i] = double.Parse(rd[n]);
            }
            if (str_x.Length != 0)
            {
                rx = str_x.Split(',');
                if (rx.Length != n)
                {
                    richTextBox4.Text = "input error! ()\n";
                    return;
                }
                for (i = 0; i < n; ++i)
                    x[i] = double.Parse(rx[i]);
            }
            else
            {
                for (i = 0; i < n; ++i)
                    x[i] = 0;
            }
            if (tag == 0)
            {
                tag = sor(a, b, x, n, e, w, 0);
                if (tag > 0)
                {
                    richTextBox4.Text = "data error!\n";
                    return;
                }
            }
            else if (tag == 1)
            {
                tag = sor(a, b, x, n, 0, w, t);
                if (tag > 0)
                {
                    richTextBox4.Text = "data error!\n";
                    return;
                }
            }
            StringBuilder mystring = new StringBuilder("result:\n");
            if (tag < 0)
                mystring.Append("迭代矩阵不收敛\n");
            else
            {
                for (i = 0; i < n; ++i)
                {
                    mystring.Append("x" + i.ToString() + ": ");
                    mystring.Append(x[i].ToString("f4") + "   ");
                }
                if (tag == 0)
                    mystring.Append("\n迭代次数： " + times.ToString() + "\n");             
            }
            richTextBox4.Text = mystring.ToString();
        }
    }
}
